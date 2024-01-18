!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/17/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Use of derived type objects as the data source
!                               for the allocatable components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k)
        integer, kind :: k

        real(k) :: x, y

        contains

        procedure :: moveTo
        procedure :: moveXY
        generic :: move => moveTo, moveXY
    end type

    type base
        type(point(4)), allocatable :: points(:,:)
    end type

    contains

    elemental subroutine moveTo (p1, p2)
        class(point(4)), intent(out) :: p1
        type(point(4)), intent(in) :: p2

        p1%x = p2%x
        p1%y = p2%y
    end subroutine

    elemental subroutine moveXY (p1, dx, dy)
    use ieee_arithmetic
        class(point(4)), intent(inout) :: p1
        real(4), intent(in) :: dx, dy

        select type (p1)
            type is (point(4))
                p1 = point(4)(p1%x+dx, p1%y+dy)

            class default
                p1%x = ieee_value (1.0, ieee_quite_nan)
                p1%y = ieee_value (1.0, ieee_quite_nan)
        end select
    end subroutine
end module

program dtparamConstr054a
use m
    type(base), allocatable :: b1

    type (point(4)), pointer :: p1(:,:)
    type(point(4)) :: p2(0:2, 2)

    logical(4), external :: precision_r4

    allocate (p1(3,2))

    p2 = point(4) (1.0, 1.0)

    b1 = base(p2)

    if ((.not. allocated(b1)) .or. (.not. allocated(b1%points))) error stop 1_4

    if (any(lbound(b1%points) /= (/0,1/)) .or. &
        any(ubound(b1%points) /= 2)) error stop 2_4

    do i = 0, 2
        do j = 1, 2
            if (.not. precision_r4(b1%points(i,j)%x, 1.0_4)) error stop 3_4

            if (.not. precision_r4(b1%points(i,j)%y, 1.0_4)) error stop 4_4

            p1(i+1,j) = point(4)(i, j)
        end do
    end do

    !! move b1%points to p1
    call b1%points%move(p1)

    do i = 0, 2
        do j = 1, 2
            if (.not. precision_r4(b1%points(i,j)%x, i*1.0_4)) error stop 5_4

            if (.not. precision_r4(b1%points(i,j)%y, j*1.0_4)) error stop 6_4
        end do
    end do

    !! move b1%points by dx, dy
    call b1%points%move(1.0_4*reshape((/(i, i=1,size(p1))/), (/3,2/)), -1.0_4)

    k = 1
    do j = 1, 2
        do i = 0, 2
            if (.not. precision_r4(b1%points(i,j)%x, (i+k)*1.0_4)) &
                error stop 7_4


            if (.not. precision_r4(b1%points(i,j)%y, (j-1)*1.0_4)) &
                error stop 8_4

            k = k + 1
        end do
    end do
end
