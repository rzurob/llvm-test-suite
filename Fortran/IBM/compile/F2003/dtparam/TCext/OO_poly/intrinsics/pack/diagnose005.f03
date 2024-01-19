! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/pack/diagnose005.f
! opt variations: -qnol

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : Diagnose test case. VECTOR shall have
!                              rank one.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = 9
    end type
end module

program diagnose005
use m
    type(Base(20,4)) :: b1(1)
    b1%i = (/-1/)

    print *, pack(b1, .TRUE., reshape((/Base(20,4)(1),Base(20,4)(2)/),(/1,1/)))
end
