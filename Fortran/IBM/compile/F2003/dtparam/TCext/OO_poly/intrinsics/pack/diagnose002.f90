! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/pack/diagnose002.f
! opt variations: -ql

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : Diagnose test case. MASK shall be of
!                              type logical.
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

    print *, pack(b1, 1, (/Base(4)(1),Base(4)(2),Base(4)(3)/))
end
