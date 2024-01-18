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
! %GROUP: dummy020.f
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
!*                                         unlimited polymorphic dummy argument (pointer and allocatable) with array with
!*                                         d) unlimited actual argument
!*                                            1) of non-abstract dynamic type 
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
      integer  :: rid
   end type

contains

   subroutine foo(a)
      class(*), pointer :: a(:)
      select type (a)
         class is (base)
            error stop 1_4
         class is (child)
            if ((a(1)%id+a(2)%id) .ne. 9)          error stop 2_4
            if ((a(1)%rid+a(2)%rid) .ne. 11)          error stop 3_4
      end select
   end subroutine

   integer function boo(a)
      class(*), allocatable :: a(:)
      select type (a)
         class is (base)
            error stop 4_4
         class is (child)
            boo=a(1)%id + a(2)%id
      end select
   end function

end module

program dummy020
   use m
   
   class(*), allocatable, target :: u1(:)
   class(*), pointer :: u2(:)
   
   class(base), allocatable :: b1(:)
   class(child), allocatable :: c1(:)
      
   allocate (b1(2), source = (/ child(3,4), child(6,7) /))
   allocate (c1(2), source = (/ child(3,4), child(6,7) /))
      
   allocate (u1(2), source = b1)
   allocate (u2(2), source = c1) 
      
   call foo(u2)
      
   if ( boo(u1) .ne. 9 )    error stop 5_4
 
end program
