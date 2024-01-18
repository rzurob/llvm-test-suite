! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/null/ptrAssignment009.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 02/27/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : MOLD is absent.
!                              Poly. Scalar.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) :: i = 8
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) :: j = 9
    end type
end module

program ptrAssignment009
use m
    class(AbstractParent(4,:)), pointer :: b1
    class(AbstractParent(4,:)), pointer :: b2

    ! b1 is undefined before and disassociated after the assignment
    b1 => null()
    if(associated(b1)) error stop 1_4

    ! b2 is undefined before and disassociated after the assignment
    b2 => null(b1)
    if(associated(b2)) error stop 2_4

    if(.NOT. same_type_as(b1, b2)) error stop 3_4

    allocate(b1, SOURCE=Base(4,20)(9))

    ! b2 is disassociated before and associated after the assignment
    b2 => b1
    if(.NOT. associated(b1)) error stop 4_4
    if(.NOT. associated(b2)) error stop 5_4
end
