! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (A test case on geometry and
!                               different shapes.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

use geometryMod
    implicit none
    type(polyGon(:)), allocatable :: pg1
    type(point(2)), allocatable :: p1(:)

    type(point(2)) center, origin

    logical(4), external :: precision_r4
    real, dimension(2) :: r1

    real, parameter :: delta = 5.e-8
    integer i, noSize


    call origin%moveTo ([0.0, 0.0])

    !! test 1: test a triangle
    noSize = 3

    allocate (polyGon(noSize) :: pg1)
    allocate (p1(noSize))

    call p1(1)%moveTo ([1.0, 1.0])
    call p1(2)%moveTo ([1.0, -1.0])
    call p1(3)%moveTo ([0.0, 1.0])

    ! setup this triangle
    pg1 = polyGon(noSize) (p1)

    !! move this triangle's centroid to origin
    call pg1%moveTo (origin)

    center = pg1%center()

    r1 = center%vertices()

    if (any(abs(r1) > delta)) error stop 1_4

    !! verify the 3 vertices
    r1 = pg1%vertices(1)%vertices()

    if ((.not. precision_r4(r1(1), 1.0_4-2/3.0_4)) .or. &
        (.not. precision_r4(r1(2), 1.0_4-1/3.0_4))) error stop 2_4

    r1 = pg1%vertices(2)%vertices()

    if ((.not. precision_r4(r1(1), 1.0_4-2/3.0_4)) .or. &
        (.not. precision_r4(r1(2), -1.0_4-1/3.0_4))) error stop 3_4

    r1 = pg1%vertices(3)%vertices()

    if ((.not. precision_r4(r1(1), -2/3.0_4)) .or. &
        (.not. precision_r4(r1(2), 1.0_4-1/3.0_4))) error stop 4_4


    !! test 2: a square
    deallocate (p1)
    noSize = 4

    allocate (p1(noSize))

    call p1(1)%moveTo([0.0, 0.0])
    call p1(2)%moveTo([1.0, 0.0])
    call p1(3)%moveTo([1.0, 1.0])
    call p1(4)%moveTo([0.0, 1.0])

    pg1 = polyGon(noSize)(p1)

    call pg1%moveTo (origin)

    !! verify center
    associate (x => pg1%center())
        r1 = x%vertices()

        if (any(abs(r1) > delta)) error stop 5_4
    end associate

    !! verify new corrdinates of 4 vertices

    do i = 1, noSize
        associate (r2 => pg1%vertices(i))
            if (any(abs(r2%diff(p1(i)) + 0.5) > delta)) error stop 6_4
        end associate
    end do
end
