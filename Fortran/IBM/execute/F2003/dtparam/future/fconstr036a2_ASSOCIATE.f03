! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr036a2.f
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
    type dataType(k1)    ! (4)
        integer, kind         :: k1
        real(k1), allocatable :: r1(:)
    end type

    type container(k2)    ! (4)
        integer, kind                    :: k2
        class(dataType(k2)), allocatable :: data1(:)
        class(dataType(k2)), allocatable :: data2(:,:)
    end type

    type, extends (dataType) :: mType(k3,n1)    ! (4,4,20)
        integer, kind :: k3
        integer, len  :: n1
    end type
end module

program fconstr036a2
use m
    class (dataType(4)), allocatable :: d1, d2(:), d3(:,:)

    real(4), allocatable :: r1(:)

    allocate (d2(2:0), r1(10:1))
    allocate (mType(4,4,20) :: d3(2, 5:4))

    associate (x => mType(4,4,20)(r1))
        if (.not. allocated(x%r1)) error stop 1_4

        if (size(x%r1) /= 0) error stop 2_4
    end associate

    associate (x => container(4)(d2, d3))
        if ((.not. allocated (x%data1)) .or. (.not. allocated (x%data2))) &
                error stop 3_4

        if (same_type_as (x%data1, x%data2)) error stop 4_4

        if ((size (x%data1) /= 0) .or. (size(x%data2) /= 0)) error stop 5_4

        select type (y => x%data1)
            type is (dataType(4))
            class default
                error stop 6_4
        end select

        select type (y => x%data2)
            type is (mType(4,4,*))
                if (any (lbound(y) /= 1)) error stop 7_4

                if (any (ubound(y) /= (/2,0/))) error stop 8_4
            class default
                error stop 10_4
        end select
    end associate
end
