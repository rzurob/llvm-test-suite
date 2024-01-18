! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_poly/intrinsics/reshape/functionReturn006.f
! opt variations: -qnol -qreuse=self -qreuse=base

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/02/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is the return value of
!*                               intrinsic function transpose().
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = 1
    end type

    type, extends(Base) :: Child(n2,k2,k3,k4,k5)    ! (20,4,20,4,4,4,4)
        integer, kind :: k2,k3,k4,k5
        integer, len  :: n2
        integer(k2)   :: j = 2
        integer(k3)   :: k = 3
        integer(k4)   :: l = 4
        integer(k5)   :: m = 5
    end type
end module

program functionReturn006
use m
    type(Base(20,4)) :: b1(10)
    b1 = (/ (Base(20,4)(i), i=1,10) /)

    print *, transpose(reshape(b1,(/3,5/),(/Base(20,4)(-1),Base(20,4)(-2)/),(/2,1/)))
end
