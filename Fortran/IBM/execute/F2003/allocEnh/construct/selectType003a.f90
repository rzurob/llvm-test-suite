!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/21/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               An array cas of selectType003.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(16), allocatable :: data(:)
    end type

    type, extends(base) :: child
        type(base), allocatable :: r1
    end type
end module

program selectType003a
use m
    class(base), allocatable :: b1(:)

    logical(4), external :: precision_r6

    allocate(child :: b1(0:99))

    select type (x => b1)
        type is (child)
            do i = 0, 99
                x(i)%data = (/(j, j=1,i)/)
                x(i)%r1 = base(x(i)%data)
            end do
    end select

    !! verify results
    do i = 0, 99
        if ((.not. allocated(b1(i)%data)) .or. &
            (size(b1(i)%data) /= i)) error stop 1_4

        do j = 1, i
            if (.not. precision_r6(b1(i)%data(j), j*1.0_16)) error stop 2_4
        end do

        select type (x => b1(i))
            class is (child)
                if ((.not. allocated(x%r1)) .or. &
                    (.not. allocated(x%r1%data))) error stop 3_4

                if (size(x%r1%data) /= i) error stop 4_4

                do j = 1, i
                    if (.not. precision_r6(x%r1%data(j), b1(i)%data(j))) &
                        error stop 5_4
                end do

            class default
                error stop 6_4
        end select
    end do
end
