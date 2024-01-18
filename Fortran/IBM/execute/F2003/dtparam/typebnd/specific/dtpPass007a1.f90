!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/30/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound: Atest case testing point
!                               module used in dtpPAss007
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

use pointMod
    type(point(2)) :: p1, p2

    class(point(:)), allocatable :: p3, p4

    logical(4), external :: precision_r4

    real, allocatable :: r1(:), r2(:)

    allocate (point(3) :: p3, p4)

    !! position p1 at (1.0, 2.0)
    call p1%moveTo ([1.0, 2.0])

    !! position p1 at (2.0, 4.0)
    call p1%moveBy ([1.0, 2.0])

    r1 = p1%vertices()

    if (size(r1) /= 2) error stop 1_4

    if ((.not. precision_r4(r1(1), 2.0_4)) .or. &
        (.not. precision_r4(r1(2), 4.0_4))) error stop 2_4


    !! position p2 at (-1.0, -2.0)
    call p2%moveTo ([-1.0, -2.0])

    r1 = p1%diff(p2)
    r2 = p2%diff(p1)

    if ((size(r1) /= 2) .or. (size(r2) /= 2)) error stop 3_4

    if ((.not. precision_r4(r1(1), 3.0_4)) .or. &
        (.not. precision_r4(r1(2), 6.0_4))) error stop 4_4

    if ((.not. precision_r4(r2(1), -3.0_4)) .or. &
        (.not. precision_r4(r2(2), -6.0_4))) error stop 5_4


    !! position p2 the same as p1
    call p2%moveBy (p1%vertices()-p2%vertices())

    r1 = p2%vertices()

    if ((.not. precision_r4(r1(1), 2.0_4)) .or. &
        (.not. precision_r4(r1(2), 4.0_4))) error stop 6_4


    !!! test part 2: 3D point
    call p3%moveTo ([1.0, 2.0, 3.0])

    call p4%moveTo ([0.0, -1.0, -2.0])

    call p4%moveBy ([1.0, 2.0, 3.0])

    r1 = p3%vertices()
    r2 = p4%vertices()

    if ((size(r1) /= 3) .or. (size(r2) /= 3)) error stop 10_4

    if ((.not. precision_r4(r1(1), 1.0_4)) .or. &
        (.not. precision_r4(r1(2), 2.0_4)) .or. &
        (.not. precision_r4(r1(3), 3.0_4))) error stop 11_4

    if ((.not. precision_r4(r2(1), 1.0_4)) .or. &
        (.not. precision_r4(r2(2), 1.0_4)) .or. &
        (.not. precision_r4(r2(3), 1.0_4))) error stop 12_4

    call p3%moveTo (p3%diff(p4))

    r1 = p3%vertices()

    if ((abs(r1(1)) > 1.0e-8) .or. &
        (.not. precision_r4(r1(2), 1.0_4)) .or. &
        (.not. precision_r4(r1(3), 2.0_4))) error stop 13_4
end
