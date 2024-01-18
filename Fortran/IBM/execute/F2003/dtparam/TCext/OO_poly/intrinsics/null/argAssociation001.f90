! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/null/argAssociation001.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : null is an actual argument. Dummy
!                              argument is a pointer or allocatable,
!                              non-poly.
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
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program argAssociation001
use m
    type(Child(4)), pointer :: b(:,:,:)
    type(Child(4)), allocatable :: c

    allocate(b(3,4,2), SOURCE=reshape((/(Child(4)(i,i-1),i=1,24)/), &
     (/3,4,2/)))
    allocate(c, SOURCE=Child(4)(2,-2))

    call sub1(null(b), null(c), null())

    contains

    subroutine sub1(arg1, arg2, arg3)
        type(Child(4)), pointer :: arg1(:,:,:)
        type(Child(4)), allocatable :: arg2
        type(Child(4)), pointer :: arg3(:)

        if(associated(arg1)) error stop 1_4
        if(allocated(arg2)) error stop 2_4
        if(associated(arg3)) error stop 3_4
    end subroutine
end
