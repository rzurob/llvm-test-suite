! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/diagnose005.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/03/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : Diagnose test case. 1<=DIM<=n where n is
!                              the rank of ARRAY.
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

program diagnose005
use m
    type(Base(4)) :: b1(3)
    b1%i = (/-1,-2,-3/)

    print *, eoshift(b1, 1, Base(4)(1), 2)
end
