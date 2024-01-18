! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/null/typeDeclaration002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 02/27/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : MOLD shall not be present in the
!                              initialization of a type declaration.
!                              Poly. Scalar or array.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(n1)    ! (20)
        integer, len :: n1
    end type

    type, extends(AbstractParent) :: Base(k1)    ! (20,4)
        integer, kind :: k1
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child(k2)    ! (20,4,4)
        integer, kind :: k2
        integer(k2)   :: j = 9
    end type
end module

program typeDeclaration002
use m
    class(AbstractParent(:)), pointer :: b1 => null()
    class(AbstractParent(:)), pointer :: b2

    class(AbstractParent(:)), pointer :: c1(:,:) => null()
    class(AbstractParent(:)), pointer :: c2(:,:)

    b2 => null(b1)
    if(associated(b1)) error stop 1_4
    if(associated(b2)) error stop 2_4

    c2 => null(c1)
    if(associated(c1)) error stop 3_4
    if(associated(c2)) error stop 4_4
end
