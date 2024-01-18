! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/6/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the actual-arg is a function result
!                               with allocatable dummy-arg used in intrinsic
!                               assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, allocatable :: data
    end type

    contains

    function baseArray (r1, b1)
        real, intent(in) :: r1(:,:)
        type(base), allocatable :: b1(:,:)

        type(base) baseArray (size(r1,1), size(r1,2))

        do j = 1, size(r1,2)
            baseArray(:,j) = [(base(r1(i,j)), i=1, size(r1,1))]
        end do

        b1 = baseArray

        do j = 1, size(r1,2)
            do i = 1, size(r1,1)
                baseArray(i,j)%data = baseArray(i,j)%data + b1(i,j)%data
            end do
        end do
    end function
end module


program dummyArg017
use m
    type(base), allocatable :: b1(:,:)

    real, allocatable :: r1(:,:)

    logical(4), external :: precision_r4

    allocate (r1(40, 25))

    call setData (r1(1,1), 1000)

    call assgnB2toB1 (b1, baseArray(r1, b1))

    if (any(shape(b1) /= [40, 25])) error stop 1_4

    k = 1

    do j = 1, 25
        do i = 1, 40
            if (.not. precision_r4 (b1(i,j)%data, 2.0_4*k)) error stop 2_4

            k = k + 1
        end do
    end do

    contains

    subroutine assgnB2toB1 (b1, b2)
        type(base), allocatable :: b1(:,:)
        type(base) b2(:,:)

        b1 = b2
    end subroutine

    subroutine setData (r, i)
        real r(*)
        integer i

        r(1:i) = [(j, j=1,i)]
    end subroutine
end
