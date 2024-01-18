! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/diagnose001.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 01/26/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : Diagnose test case. ARRAY shall not be
!                              scalar.
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

program diagnose001
use m
    type(Base(4)) :: b1

    print *, cshift(b1, 1)
end
