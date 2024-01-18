!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 28, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of renaming the operator in
!*                               a USE statement. Intrinsic operators are not allowed
!*                               to be renamed
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module mod

  type real_num
    real :: x
  end type

  interface operator (+)
    procedure plus
  end interface


  contains
    function plus(a,b)
      type(real_num) :: plus
      type(real_num), intent(in) :: a,b
      plus%x = a%x+b%x
    end function plus
end module

module mod2
use mod , operator(*) => operator(+)
end module

program main
use mod  , only: operator(.addition.) => operator(+), real_num

  type(real_num) :: a,b,c
  a%x=1.0
  b%x=2.0
  c=a.addition.b

end program
