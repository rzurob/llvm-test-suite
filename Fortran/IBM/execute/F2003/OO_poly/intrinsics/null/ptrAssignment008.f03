!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 02/27/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : MOLD is absent.
!                              Non-poly. Array.
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

program ptrAssignment008
use m
    type(Base), pointer :: b1(:)
    type(Base), pointer :: b2(:)

    ! b1 is undefined before and disassociated after the assignment
    b1 => null()
    if(associated(b1)) error stop 1_4

    ! b2 is undefined before and disassociated after the assignment
    b2 => b1
    if(associated(b2)) error stop 2_4

    if(.NOT. same_type_as(b1, b2)) error stop 3_4
end