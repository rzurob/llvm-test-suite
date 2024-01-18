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
!*                               a USE statement. If the operator has the private
!*                               attribute it cannot be renamed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module mod

  type real_num
    real :: x
  end type

  interface operator (.add.)
    procedure plus
  end interface

  private :: operator(.add.)

  contains
    function plus(a,b)
      type(real_num) :: plus
      type(real_num), intent(in) :: a,b
      plus%x = a%x+b%x
    end function plus
end module



program main
use mod  , operator(.plus.) => operator(.addition.) !error, operator cannot have private attribute

  type(real_num) :: a,b,c
  a%x=1.0
  b%x=2.0
  c=a.plus.b

end program
