! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/pack/diagnose004.f
! opt variations: -ql

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : Diagnose test case. VECTOR shall be of
!                              the same type and type parameters as
!                              ARRAY.
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
    type(Base(4)) :: b1(2)
    b1%i = (/-1,-2/)

    print *, pack(b1,.TRUE.,(/"a","b","c"/))
end
