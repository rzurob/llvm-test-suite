! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/OO_poly/intrinsics/pack/diagnose007.f
! opt variations: -qnol -qreuse=none

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = 9
    end type

    type Base1(n2,k2)    ! (20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)   :: i = 8
        integer(k2)   :: j = 9
    end type
end module

program diagnose007
use m
    type(Base(20,4)) :: b1(2)
    b1%i = (/-1,-2/)

    print *, pack(b1,.TRUE.,(/Base1(20,4)(1,2),Base1(20,4)(3,4),Base1(20,4)(5,6)/))
end
