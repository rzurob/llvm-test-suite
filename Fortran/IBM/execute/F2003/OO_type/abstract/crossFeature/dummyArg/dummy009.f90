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
! %GROUP: dummy009.f
! %VERIFY: dummy009.out:dummy009.vf
! %STDIN:
! %STDOUT: dummy009.out
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
!*                                         b) OPTIONAL attribute with polymorphic abstract type (non-pointer and non-allocatable)
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

   subroutine foo(a, b)
      class(base) :: a
      class(base), optional :: b
      
      if (a%id .ne. 3) error stop 1_4
      if ( present(b) ) then
         if (b%id .ne. 3) error stop 2_4
         print *,"foo: present(b)"
      else
         print *,"foo: not present(b)"      
      end if
      
   end subroutine

   integer function boo(a, b)
      class(base) :: a
      class(base), optional :: b
      if ( present(b) ) then
         boo = a%id + b%id
      else
         boo = 30
      end if
   end function

end module

program dummy009
   use m
   
   class(base), allocatable :: b1
   type(child), target :: c1 = child(3,3.4)
   class(child), pointer :: c2
   
   allocate (b1, source = child(3,2.3))
   c2 => c1
      
   call foo(b1)
   call foo(c1)
   call foo(c2)
   
   if ( boo(b1) .ne. 30 ) error stop 3_4
   if ( boo(c1) .ne. 30 ) error stop 4_4
   if ( boo(c2) .ne. 30 ) error stop 5_4
   
   call foo(b1, b1)
   call foo(b1, c1)
   call foo(c1, c2)
   
   if ( boo(b1, b1) .ne. 6 ) error stop 6_4
   if ( boo(b1, c1) .ne. 6 ) error stop 7_4
   if ( boo(c1, c2) .ne. 6 ) error stop 8_4
   
end program
