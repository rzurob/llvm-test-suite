 !#######################################################################
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummy003.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*					   dummy argument defined to be abstract type.
!*                                         iii) polymorphic abstract type dummy argument (pointer or allocatable) with
!*                                              a) polymorphic abstract type actual argument
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
   end type

contains

   subroutine foo(a)
      class(base), pointer :: a
      if (.not. associated(a) ) error stop 1_4
      if (a%id .ne. 3)          error stop 2_4
   end subroutine

   integer function boo(a)
      class(base), allocatable :: a
      boo = a%id
   end function

end module

program dummy003
   use m
   class(base), allocatable, target :: b1
   class(base), pointer :: b2

   allocate ( b1, source = child(3) )

   b2 => b1

   call foo(b2)

   if ( boo(b1) .ne. 3 ) error stop 3_4

end program