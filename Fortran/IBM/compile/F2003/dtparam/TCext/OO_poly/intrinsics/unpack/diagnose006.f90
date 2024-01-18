! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_poly/intrinsics/unpack/diagnose006.f
! opt variations: -qnol -qreuse=self

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Diagnose test case. FIELD shall be of
!                              the same type and type parameters as
!                              VECTOR.
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

    type Base1(n2,k2,k3)    ! (20,4,4)
        integer, kind :: k2,k3
        integer, len  :: n2
        integer(k2)   :: i = 8
        integer(k3)   :: j = 9
    end type
end module

program diagnose006
use m
    type(Base(20,4)) :: b1(2)
    b1%i = (/-1,-2/)

    print *, unpack(b1, (/.TRUE.,.FALSE./), (/Base1(20,4,4)(1,2),Base1(20,4,4)(3,4)/))
end
