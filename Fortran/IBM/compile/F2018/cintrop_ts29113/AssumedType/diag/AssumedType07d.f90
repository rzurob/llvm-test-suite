!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : TYPE(*) not a dummy argument
!*       - variable declaration (local / module) with and without BIND(C)
!*       - type declaration (component of type TYPE(*) ) with and without BIND(C)
!*       - variable with save attribute
!*       - volatile attribute
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module type_mod
  implicit none

  type :: dt0
      type(*) :: cmp0
  end type

  type, bind(c) :: dt1
      type(*) :: cmp1
  end type
end module type_mod
module mod
  implicit none

  type(*)               :: mod_wrong1
  type(*), bind(c)      :: mod_wrong2
  type(*), save         :: mod_wrong3
  type(*), target       :: mod_wrong4
  type(*), pointer      :: mod_wrong5
  type(*), allocatable  :: mod_wrong6
  type(*), volatile     :: mod_wrong7
contains
  subroutine sub(a, b, c, d, e, f, g, h, i)
    type(*), bind(c)       :: a ! error
    type(*), value         :: b ! error
    type(*), intent(out)   :: c ! error
    type(*), pointer       :: d ! error
    type(*), allocatable   :: e ! error
    type(*), intent(in)    :: f ! ok
    type(*), intent(inout) :: g ! ok
    type(*), dimension(10) :: h ! error
    type(*)                :: i(10) ! error
  end subroutine
end module mod
program AssumedType07d
implicit none
type(*)               :: wrong1
type(*), bind(c)      :: wrong2
type(*), save         :: wrong3
type(*), target       :: wrong4
type(*), pointer      :: wrong5
type(*), allocatable  :: wrong6
type(*), volatile     :: wrong7

end program AssumedType07d
