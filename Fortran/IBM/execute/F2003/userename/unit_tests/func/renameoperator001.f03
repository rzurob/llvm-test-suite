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
!*  DESCRIPTION                : functional testing of renaming the operator in
!*                               a USE statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mod

  type real_num
    real :: x
  end type

  interface operator (.add.)
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
use mod, operator(.addition.)=>operator(.add.) , real_number => real_num

end module

module mod3
use mod , only: operator(.addition.) => operator(.add.)
end module

program main
use mod2  , operator(.plus.) => operator(.addition.)

  type(real_number) :: a,b,c
  a%x=1.0
  b%x=2.0
  c=a.plus.b

end program
