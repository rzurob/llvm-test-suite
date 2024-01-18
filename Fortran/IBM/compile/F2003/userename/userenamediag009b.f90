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
!*  DESCRIPTION                : ensure switchign target and destination yields error
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  interface operator(.add.)
    module procedure plus
  end interface

  contains
    function plus(a,b)
      real :: plus
      real, intent(in) :: a,b
      plus = a+b
    end function plus

end module


program main
  use opmod ,  operator(.PlUs.) => operator(.add.)
  interface operator(.plus.)
    function plus2(a,b)
    real :: plus2
    real, intent(in) :: a,b
    end function
  end interface

  real :: a,b,c
  c=a.plus.b
  b=a.Plus.c
  a=b.pLuS.c

end program

function plus2(a,b)
      real :: plus2
      real, intent(in) :: a,b
      plus2 = a+b
end function plus2