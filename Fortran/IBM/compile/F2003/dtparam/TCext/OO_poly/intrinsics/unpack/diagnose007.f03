! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=self /tstdev/OO_poly/intrinsics/unpack/diagnose007.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/21/2005
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

    type Base1(n2,k2)    ! (20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)   :: i = 8
        integer(k2)   :: j = 9
    end type
end module

program diagnose007
use m
    class(Base(:,4)), pointer :: b1(:)
    class(Base1(:,4)), allocatable :: f1(:)
    allocate(b1(2), SOURCE=(/Base(20,4)(-1), Base(20,4)(-2)/))
    allocate(f1(3), SOURCE=(/Base1(20,4)(1,2),Base1(20,4)(3,4),Base1(20,4)(5,6)/))

    select type(name1=>unpack(b1,(/.TRUE.,.FALSE.,.TRUE./),f1))
        class default
            error stop 1_4
    end select
end
