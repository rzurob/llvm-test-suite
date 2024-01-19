! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/null/argAssociation006.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : MOLD is a dummy argument. Dummy
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

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program argAssociation006
use m
    class(*), pointer :: b
    class(*), allocatable :: c(:,:)

    allocate(b, SOURCE=Child(4)(1,2))
    allocate(c(2,3), SOURCE=reshape((/(Child(4)(i,i+2),i=1,5)/), &
     (/2,3/), (/Child(4)(1,-1)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(*), pointer :: arg1
        class(*), allocatable :: arg2(:,:)

        if(.NOT. associated(arg1)) error stop 1_4
        if(.NOT. allocated(arg2)) error stop 2_4

        if(associated(null(arg1))) error stop 3_4
        if(allocated(null(arg2))) error stop 4_4

        if(.NOT. same_type_as(arg1, arg2)) error stop 5_4
        if(same_type_as(null(arg1), null(arg2))) error stop 6_4
end subroutine
end
