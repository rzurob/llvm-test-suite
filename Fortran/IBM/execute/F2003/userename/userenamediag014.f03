!**********************************************************************
!*  ===================================================================
!*
!Mar. 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : ensure renaming to an ambiguous private type bound yields error
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  type modreal
    real :: x

    contains
      procedure :: plus
      procedure :: plus2
      generic, private :: operator(.add.) => plus

  end type

  interface operator(.adda.)
    module procedure plus2
  end interface

  contains
    function plus(a,b)
      type(modreal) :: plus
      class(modreal), intent(in) :: a,b
      plus%x = a%x+b%x*2.0
    end function plus

    function plus2(a,b)
      type(modreal) :: plus2
      class(modreal), intent(in) :: a,b
      plus2%x = a%x+b%x
    end function plus2


end module


program main
use opmod , operator(.add.) => operator(.adda.)
  type(modreal) :: a,b,c
  logical(4), external :: precision_r4

  a%x = 1.0
  b%x = 2.0

  c=a.add.b

  if (.not. precision_r4(c%x, 3.0)) error stop 1_4
end program