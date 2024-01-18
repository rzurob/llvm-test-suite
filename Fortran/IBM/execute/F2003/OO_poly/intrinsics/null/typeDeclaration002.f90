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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type
end module

program typeDeclaration002
use m
    class(AbstractParent), pointer :: b1 => null()
    class(AbstractParent), pointer :: b2

    class(AbstractParent), pointer :: c1(:,:) => null()
    class(AbstractParent), pointer :: c2(:,:)

    b2 => null(b1)
    if(associated(b1)) error stop 1_4
    if(associated(b2)) error stop 2_4

    c2 => null(c1)
    if(associated(c1)) error stop 3_4
    if(associated(c2)) error stop 4_4
end
