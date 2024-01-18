!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/18/2005
!*
!*  DESCRIPTION                : structure constructor (zero-size arrays as the
!                               component data expression)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType
        real(4), allocatable :: r1(:)
    end type

    type container
        class (dataType), allocatable :: data1(:)
        class (dataType), allocatable :: data2(:,:)
    end type

    type, extends (dataType) :: mType
    end type
end module

program fconstr036a2
use m
    class (dataType), allocatable :: d1, d2(:), d3(:,:)

    real(4), allocatable :: r1(:)

    allocate (d2(2:0), r1(10:1))
    allocate (mType :: d3(2, 5:4))

    associate (x => mType(r1))
        if (.not. allocated(x%r1)) error stop 1_4

        if (size(x%r1) /= 0) error stop 2_4
    end associate

    associate (x => container(d2, d3))
        if ((.not. allocated (x%data1)) .or. (.not. allocated (x%data2))) &
                error stop 3_4

        if (same_type_as (x%data1, x%data2)) error stop 4_4

        if ((size (x%data1) /= 0) .or. (size(x%data2) /= 0)) error stop 5_4

        select type (y => x%data1)
            type is (dataType)
            class default
                error stop 6_4
        end select

        select type (y => x%data2)
            type is (mType)
                if (any (lbound(y) /= 1)) error stop 7_4

                if (any (ubound(y) /= (/2,0/))) error stop 8_4
            class default
                error stop 10_4
        end select
    end associate
end
