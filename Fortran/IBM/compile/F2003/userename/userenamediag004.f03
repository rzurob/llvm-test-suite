!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : ensure langlvl message is issued
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  type modreal
    real :: x
  end type


  private :: operator(+)

  interface operator(+)
    module procedure plus
  end interface

  contains
    function plus(a,b)
      type(modreal) :: plus
      class(modreal), intent(in) :: a,b
      plus%x = a%x+b%x
    end function plus


end module


program main
use opmod , operator(.plus.) => operator(+)
  type(modreal) :: a,b,c
  c=a+b
end program