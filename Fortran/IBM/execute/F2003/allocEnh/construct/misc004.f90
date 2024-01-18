! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/6/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               recursive defined assignment
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
        class(point), intent(inout) :: p1
        real, intent(in) :: r(:)

        if (size(r) <= 0) stop 10

        p1%x = [p1%x, r]

        if (size(r) > 1) p1 = r(:size(r) -1)
    end subroutine
end module

use m
    type(point), allocatable :: p1

    real, allocatable :: r1(:)

    logical(4), external :: precision_r4

    p1 = point([real::])

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

end
