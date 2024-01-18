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
!                              non-poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program argAssociation004
use m
    type(Child), pointer :: b(:,:,:)
    type(Child), allocatable :: c

    allocate(b(3,4,2), SOURCE=reshape((/(Child(i,i-1),i=1,24)/), &
     (/3,4,2/)))
    allocate(c, SOURCE=Child(2,-2))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Child), pointer :: arg1(:,:,:)
        type(Child), allocatable :: arg2

        if(.NOT. associated(arg1)) error stop 1_4
        if(.NOT. allocated(arg2)) error stop 2_4

        if(associated(null(arg1))) error stop 3_4
        if(allocated(null(arg2))) error stop 4_4

        if(.NOT. same_type_as(arg1, arg2)) error stop 5_4
        if(.NOT. same_type_as(null(arg1), null(arg2))) error stop 6_4
    end subroutine
end
