! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/null/ptrAssignment011.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 02/27/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : MOLD is absent.
!                              Unlimited poly. Scalar.
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

program ptrAssignment011
use m
    class(*), pointer :: b1
    class(*), pointer :: b2

    ! b1 is undefined before and disassociated after the assignment
    b1 => null()
    if(associated(b1)) error stop 1_4

    ! b2 is undefined before and disassociated after the assignment
    b2 => b1
    if(associated(b2)) error stop 2_4

    if(same_type_as(b1, b2)) error stop 3_4
end