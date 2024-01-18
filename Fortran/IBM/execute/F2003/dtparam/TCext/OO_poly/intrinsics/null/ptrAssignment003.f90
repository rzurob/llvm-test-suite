! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/null/ptrAssignment003.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 02/27/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : MOLD is present.
!                              IF MOLD is a pointer, its pointer
!                              association status may be undefined,
!                              disassociated, or associated.
!                              Poly. Scalar.
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

program ptrAssignment003
use m
    class(AbstractParent(:)), pointer :: b1
    class(AbstractParent(:)), pointer :: b2

    ! b1 is undefined, b2 is disassociated after the assignment
    b2 => null(b1)
    if(associated(b2)) error stop 1_4

    ! b1 is undefined before and disassociated after the assignment
    b1 => null(b1)
    if(associated(b1)) error stop 2_4

    if(.NOT. same_type_as(b1, b2)) error stop 3_4

    allocate(b1, SOURCE=Base(20,4)(9))

    ! b2 is disassociated before and associated after the assignment
    b2 => b1
    if(.NOT. associated(b1)) error stop 4_4
    if(.NOT. associated(b2)) error stop 5_4
end
