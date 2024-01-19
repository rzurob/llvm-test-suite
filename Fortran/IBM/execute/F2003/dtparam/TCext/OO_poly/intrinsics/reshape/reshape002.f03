! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/reshape/reshape002.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is non-poly
!*    PAD and ORDER are specified. PAD has same declared type as SOURCE
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

program reshape002
use m
    type(Base(20,4)) :: b1(10)
    type(Base(20,4)) :: b2(3,5)
    b1 = (/ (Base(20,4)(i), i=1,10) /)
    b2(:,1) = (/ Base(20,4)(21),Base(20,4)(22),Base(20,4)(23) /)
    b2(:,2) = (/ Base(20,4)(24),Base(20,4)(25),Base(20,4)(26) /)
    b2(:,3) = (/ Base(20,4)(27),Base(20,4)(28),Base(20,4)(29) /)
    b2(:,4) = (/ Base(20,4)(30),Base(20,4)(31),Base(20,4)(32) /)
    b2(:,5) = (/ Base(20,4)(33),Base(20,4)(34),Base(20,4)(35) /)

    b2 = reshape(b1, (/3,5/), (/Base(20,4)(0),Base(20,4)(1)/), (/2,1/))

    print *, b1
    print *, b2
end
