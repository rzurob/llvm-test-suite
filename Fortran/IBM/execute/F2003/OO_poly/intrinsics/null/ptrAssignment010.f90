!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 02/27/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : MOLD is absent.
!                              Poly. Array.
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

program ptrAssignment010
use m
    class(AbstractParent), pointer :: b1(:,:)
    class(AbstractParent), pointer :: b2(:,:)

    ! b1 is undefined before and disassociated after the assignment
    b1 => null()
    if(associated(b1)) error stop 1_4

    ! b2 is undefined before and disassociated after the assignment
    b2 => null(b1)
    if(associated(b2)) error stop 2_4

    if(.NOT. same_type_as(b1, b2)) error stop 3_4

    allocate(b1(2,3), SOURCE=Child(1,2))

    ! b2 is disassociated before and associated after the assignment
    b2 => b1
    if(.NOT. associated(b1)) error stop 4_4
    if(.NOT. associated(b2)) error stop 5_4

    if(.NOT. same_type_as(b1, Child(1,1))) error stop 6_4
    if(.NOT. same_type_as(b1, b2)) error stop 7_4
end
