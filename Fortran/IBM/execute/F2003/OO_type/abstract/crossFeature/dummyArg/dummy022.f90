 !#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummy022.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type		    
!*                                         b-a) OPTIONAL attribute with polymorphic abstract type (non-pointer and non-allocatable) with array
!*                                            1) shall or shall not have actual argument associated with it
!*                                            2) if actual argument is associated, try
!*                                               i) polymorphic abstract type actual argument
!*                                               ii) polymorphic extension type of abstract type actual argument
!*                                               iii) non-polymorphic extension type of abstract type actual argument
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m
   
   type, abstract :: base
      integer :: id
   end type
   
   type, extends(base) :: child
      real :: rid
   end type

contains

   subroutine foo(a,b)
      class(base) :: a(:)
      class(base), optional :: b(2,*)
      
      if ((a(1)%id+a(2)%id) .ne. 9) error stop 1_4
      
      if (present(b) ) then
         if ((b(1,1)%id+b(2,1)%id) .ne. 9) error stop 2_4
      end if
      
   end subroutine

   integer function boo(a, b)
      class(base) :: a(2)
      class(base), optional :: b(:)
      if ( present(b) ) then
         boo = (a(1)%id+a(2)%id) + (b(1)%id+b(2)%id)
      else
         boo = (a(1)%id+a(2)%id)
      end if
   end function

end module

program dummy022
   use m
   
   class(base),  allocatable         :: b1(:)
   type(child),  allocatable, target :: c1(:)
   class(child), pointer            :: c2(:)
   
   allocate (b1(2), source = (/ child(3,4.5), child(6,7.8) /))
   allocate (c1(2), source = (/ child(3,4.5), child(6,7.8) /))
 
   c2 => c1
      
   call foo(b1)
   call foo(c1)
   call foo(c2)
   
   if ( boo(b1) .ne. 9 ) error stop 3_4
   if ( boo(c1) .ne. 9 ) error stop 4_4
   if ( boo(c2) .ne. 9 ) error stop 5_4
   
   call foo(b1, b1)
   call foo(b1, c1)
   call foo(c1, c2)
   
   if ( boo(b1, b1) .ne. 18 ) error stop 6_4
   if ( boo(b1, c1) .ne. 18 ) error stop 7_4
   if ( boo(c1, c2) .ne. 18 ) error stop 8_4
   
end program
