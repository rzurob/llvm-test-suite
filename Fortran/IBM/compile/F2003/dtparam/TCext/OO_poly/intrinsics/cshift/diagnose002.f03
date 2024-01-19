! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/diagnose002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 01/27/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : Diagnose test case. SHIFT shall be of
!                              type integer.
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

program diagnose002
use m
    type(Base(4)) :: b1(2)

    print *, cshift(b1, 1.0)
end
