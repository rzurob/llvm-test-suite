! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/null/argAssociation003.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : null is an actual argument. Dummy
!                              argument is a pointer or allocatable,
!                              unlimited poly.
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

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program argAssociation003
use m
    class(*), pointer :: b
    class(*), allocatable :: c(:,:)

    allocate(b, SOURCE=Child(4,4)(1,2))
    allocate(c(2,3), SOURCE=reshape((/(Child(4,4)(i,i+2),i=1,5)/), &
     (/2,3/), (/Child(4,4)(1,-1)/), (/2,1/)))

    call sub1(null(b), null(c), null())

    if(.NOT. same_type_as(b, c)) error stop 7_4

    contains

    subroutine sub1(arg1, arg2, arg3)
        class(*), pointer :: arg1
        class(*), allocatable :: arg2(:,:)
        class(*), pointer :: arg3(:)

        if(associated(arg1)) error stop 1_4
        if(allocated(arg2)) error stop 2_4
        if(associated(arg3)) error stop 3_4

        if(same_type_as(arg1, arg2)) error stop 4_4
        if(same_type_as(arg2, arg3)) error stop 5_4
        if(same_type_as(arg3, arg1)) error stop 6_4
end subroutine
end