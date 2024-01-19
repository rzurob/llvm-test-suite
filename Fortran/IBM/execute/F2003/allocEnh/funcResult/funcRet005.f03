! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/16/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test function result of derived type arrays:
!                               derived type with array component of derived
!                               type of allocatable componnet.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), allocatable :: data(:)
    end type

    type container
        type(base) :: data(2)
    end type

    contains

    type(container) function splitRealArray (d1)
        double precision, intent(in) :: d1(:,:,:)

        dimension splitRealArray(3)

        call oneDimension (d1(:,1,1), splitRealArray(1))
        call oneDimension (d1(1,:,1), splitRealArray(2))
        call oneDimension (d1(1,1,:), splitRealArray(3))


        contains

        subroutine oneDimension (d2, co1)
            double precision, intent(in) :: d2(:)
            type(container), intent(out) :: co1

            double precision, allocatable :: average

            if (size(d2) == 0) return

            average = sum(d2) / size(d2)

            co1%data(1)%data = pack (d2, d2 > average)
            co1%data(2)%data = pack (d2, d2 <= average)
        end subroutine
    end function
end module

program funcRet005
use m
    type(container), allocatable :: co1(:)
    real(8), allocatable :: d1(:,:,:)

    allocate (d1(10,10,10))

    d1 = reshape([(j, j=1,10**3)], [10,10,10])

    !! test 1
    co1 = splitRealArray (d1)

    !! verify co1
    if (size(co1) /= 3) error stop 1_4

    do i = 1, 3
        write (*, '(10f15.6)') co1(i)%data(1)%data
        write (*, '(10f15.6)') co1(i)%data(2)%data
    end do

    !! test 2
    co1 = [(container(base(null())), j=1,4)]

    co1(3:1:-1) = splitRealArray (d1)

    if (size(co1) /= 4) error stop 2_4

    do i = 1, 3
        write (*, '(10f15.6)') co1(i)%data(1)%data
        write (*, '(10f15.6)') co1(i)%data(2)%data
    end do

    if (allocated(co1(4)%data(1)%data) .or. &
        allocated(co1(4)%data(2)%data)) error stop 3_4
end

