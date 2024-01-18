! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/null/argAssociation005.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : MOLD is a dummy argument. Dummy
!                              argument is a pointer or allocatable,
!                              poly.
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

program argAssociation005
use m
    class(Base(4)), pointer :: b
    class(Child(4)), allocatable :: c(:,:)

    allocate(Child(4)::b)
    allocate(c(2,3), SOURCE=reshape((/(Child(4)(i,i+2),i=1,5)/), &
     (/2,3/), (/Child(4)(1,-1)/), (/2,1/)))

    call sub1(b, c, null())

    contains

    subroutine sub1(arg1, arg2, arg3)
        class(Base(4)), pointer :: arg1
        class(Child(4)), allocatable :: arg2(:,:)
        class(Base(4)), pointer :: arg3(:)

        if(.NOT. associated(arg1)) error stop 1_4
        if(.NOT. allocated(arg2)) error stop 2_4
        if(associated(arg3)) error stop 3_4

        if(associated(null(arg1))) error stop 4_4
        if(allocated(null(arg2))) error stop 5_4
        if(associated(null(arg3))) error stop 6_4

        if(.NOT. same_type_as(arg1, arg2)) error stop 7_4
        if(same_type_as(arg2, arg3)) error stop 8_4
        if(same_type_as(arg3, arg1)) error stop 9_4

        if(same_type_as(null(arg1), null(arg2))) error stop 10_4
        if(same_type_as(null(arg2), null(arg3))) error stop 11_4
        if(.NOT. same_type_as(null(arg3), null(arg1))) error stop 12_4
    end subroutine
end
