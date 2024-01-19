! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/reshape/reshape022.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/05/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is non-poly
!*    PAD and ORDER are not specified
!*    SOURCE is rank two.
!*
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
        integer(k1)      i
    end type
end module

program reshape022
use m
    type(Base(20,4)) :: b1(20)
    type(Base(20,4)) :: b2(6,4)
    type(Base(20,4)) :: b3(3,5)

    b1 = (/ (Base(20,4)(i), i=1,20) /)

    b2 = reshape(b1, (/6,4/), (/Base(20,4)(-1),Base(20,4)(-2)/), (/2,1/))

    b3 = reshape(b2, (/3,5/), (/Base(20,4)(-3),Base(20,4)(-4)/), (/2,1/))

    print *, b1
    print *, b2
    print *, b3
end
