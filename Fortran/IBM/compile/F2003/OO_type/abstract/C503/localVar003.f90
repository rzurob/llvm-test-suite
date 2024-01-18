!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp localVar003.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: non-polymorphic abstract type entities in functions and subroutines (scalar, array, pointer, allocatable)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type, abstract:: base
      integer :: i = 5
   end type

   type, extends(base) :: child
   end type

contains

   integer function foo ()
      type(base) :: b1
      type(base), dimension(5) :: b2
      type(base), pointer :: b3
      type(base), allocatable, dimension(:) :: b4
      foo = 5
   end function

end module

program localVar003
end program

subroutine foo ()
   use m, only: base, newbase=>base
   type(newbase) :: b1
   type(newbase), dimension(5) :: b2
   call innerfoo()
contains
   subroutine innerfoo()
      type(newbase), pointer :: b3
      type(newbase), allocatable, dimension(:) :: b4
   end subroutine
end subroutine