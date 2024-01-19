! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/null/ptrAssignment002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 02/27/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : MOLD is present.
!                              IF MOLD is a pointer, its pointer
!                              association status may be undefined,
!                              disassociated, or associated.
!                              Non-poly. Array.
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

program ptrAssignment002
use m
    type(Base(4)), pointer :: b1(:)
    type(Base(4)), pointer :: b2(:)

    ! b1 is undefined, b2 is disassociated after the assignment
    b2 => null(b1)
    if(associated(b2)) error stop 1_4

    ! b1 is undefined before and disassociated after the assignment
    b1 => null(b1)
    if(associated(b1)) error stop 2_4

    if(.NOT. same_type_as(b1, Base(4)(1))) error stop 3_4
    if(.NOT. same_type_as(b1, b2)) error stop 4_4

    allocate(b1(10), SOURCE=Base(4)(9))

    ! b2 is disassociated before and associated after the assignment
    b2 => b1
    if(.NOT. associated(b1)) error stop 5_4
    if(.NOT. associated(b2)) error stop 6_4
end
