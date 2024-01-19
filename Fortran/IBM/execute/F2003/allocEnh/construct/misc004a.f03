! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/6/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               recursive defined assignment; also use
!                               move_alloc for the assignment
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real, allocatable :: x(:)

        contains

        procedure :: assgnReal
        generic :: assignment(=) => assgnReal
    end type

    contains

    recursive subroutine assgnReal (p1, r)
        class(point), intent(out) :: p1
        real, intent(in) :: r(:)

        real, allocatable, save :: temp(:)


        if (size(r) >= 1) then
            if (.not. allocated(temp)) then
                temp = r
            else
                temp = [temp, r]
            end if

            p1%x = temp

            p1 = r(:size(r) -1)
        else
            call move_alloc(temp, p1%x)
        end if
    end subroutine
end module

use m
    type(point), allocatable :: p1

    real, allocatable :: r1(:)

    logical(4), external :: precision_r4

    p1 = point([real:: 1,2,3,4,5,6,7,8,9,10])

    r1 = [(i, i=1, 500)]

    p1 = r1

    if (size(p1%x) /= 125250) error stop 1_4

    k = 1

    do i = 1, 500
        do j = 1, 501-i
            if (.not. precision_r4 (p1%x(k), j*1.0)) error stop 2_4

            k = k + 1
        end do
    end do

    p1 = r1(20:1:-2)

    if (size(p1%x) /= 55) error stop 3_4

    k = 1

    do i = 1, 10
        do j = 10, i, -1
            if (.not. precision_r4(p1%x(k), j*2.0)) error stop 4_4

            k = k + 1
        end do
    end do
end
