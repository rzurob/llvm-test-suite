! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/diagnose004.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/03/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : Diagnose test case. DIM shall be of type
!                              integer.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i = 9
    end type
end module

program diagnose004
use m
    type(Base(4)) :: b1(3)
    b1%i = (/-1,-2,-3/)

    print *, eoshift(b1, 1, Base(4)(1), 1.0)
end
