!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 02/27/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : MOLD shall not be present in the
!                              initialization of a type declaration.
!                              Non-poly. Scalar or array.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer :: i = 9
    end type
end module

program typeDeclaration001
use m
    type(Base), pointer :: b1 => null()
    type(Base), pointer :: b2

    type(Base), pointer :: c1(:,:) => null()
    type(Base), pointer :: c2(:,:)

    b2 => null(b1)
    if(associated(b1)) error stop 1_4
    if(associated(b2)) error stop 2_4

    c2 => null(c1)
    if(associated(c1)) error stop 3_4
    if(associated(c2)) error stop 4_4
end
