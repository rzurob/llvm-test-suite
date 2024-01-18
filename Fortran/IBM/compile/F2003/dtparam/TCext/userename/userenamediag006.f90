! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/userename/userenamediag006.f
! opt variations: -qnol

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
!*  DESCRIPTION                : ensure private op can't be accessed even with ONLY
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  type modreal(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    real(k1)      :: x
  end type


  private :: operator(.add.)

  interface operator(.add.)
    module procedure plus
  end interface

  contains
    function plus(a,b)
      type(modreal(20,4)) :: plus
      class(modreal(*,4)), intent(in) :: a,b
      plus%x = a%x+b%x
    end function plus


end module


program main
use opmod , only: operator(.plus.) => operator(.add.)
  type(modreal) :: a,b,c
  c=a.plus.b
end program
