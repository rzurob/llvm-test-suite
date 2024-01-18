!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/01/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (2nd test case for defect 336171)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module pointMod
    type point2! (n)
        private

        integer :: n = 2
        real :: coord(2)

        contains

        procedure :: moveTo => moveTo2
        procedure :: moveBy => moveBy2
        procedure :: vertices => vertices2
        procedure :: diff => pointDiff2
    end type

    type point3! (n)
        private

        integer :: n = 3
        real :: coord(3)

        contains

        procedure :: moveTo => moveTo3
        procedure :: moveBy => moveBy3
        procedure :: vertices => vertices3
        procedure :: diff => pointDiff3
    end type

    contains

    subroutine moveTo2 (p1, r1)
        class(point2), intent(inout) :: p1
        real, intent(in) :: r1(p1%n)

        p1%coord = r1
    end subroutine


    subroutine moveTo3 (p1, r1)
        class(point3), intent(inout) :: p1
        real, intent(in) :: r1(p1%n)

        p1%coord = r1
    end subroutine

    subroutine moveBy2 (p1, r1)
        class(point2), intent(inout) :: p1
        real, intent(in) :: r1(p1%n)

        call p1%moveTo (r1+p1%vertices())
    end subroutine

    subroutine moveBy3 (p1, r1)
        class(point3), intent(inout) :: p1
        real, intent(in) :: r1(p1%n)

        call p1%moveTo (r1+p1%vertices())
    end subroutine

    function vertices2 (p1)
        class(point2), intent(in) :: p1

        real, dimension(p1%n) :: vertices2

        vertices2 = p1%coord
    end function

    function vertices3 (p1)
        class(point3), intent(in) :: p1

        real, dimension(p1%n) :: vertices3

        vertices3 = p1%coord
    end function

    function pointDiff2 (p1, p2)
        class(point2), intent(in) :: p1, p2

        real, dimension(p1%n) :: pointDiff2

        if (p1%n /= p2%n) stop 10

        pointDiff2 = p1%vertices() - p2%vertices()
    end function

    function pointDiff3 (p1, p2)
        class(point3), intent(in) :: p1, p2

        real, dimension(p1%n) :: pointDiff3

        if (p1%n /= p2%n) stop 10

        pointDiff3 = p1%vertices() - p2%vertices()
    end function
end module


module shapeMod
use pointMod
    type, abstract :: shape
        contains

        procedure(absInf1), pass(s1), deferred :: center
        procedure(absInf2), pass(s1), deferred :: moveTo
    end type

    abstract interface
        type(point2) function absInf1 (s1)
            import
            class(shape), intent(in) :: s1
        end function

        subroutine absInf2 (s1, where)
            import
            class(shape), intent(inout) :: s1
            type(point2), intent(in) :: where
        end subroutine
    end interface
end module


module geometryMod
use shapeMod
    type, extends(shape) :: polyGon3! (n)
        integer :: n = 3

        type(point2) :: vertices(3)

        contains

        procedure :: center => polygonCenter3
        procedure :: moveTo => movePolygon3
    end type

    type, extends(shape) :: polyGon4! (n)
        integer :: n = 4

        type(point2) :: vertices(4)

        contains

        procedure :: center => polygonCenter4
        procedure :: moveTo => movePolygon4
    end type

    contains

    type(point2) function polygonCenter3 (s1)
        class(polyGon3), intent(in) :: s1

        real tempArray(2)

        tempArray = 0

        do i = 1, s1%n
            tempArray = tempArray + s1%vertices(i)%vertices()
        end do

        tempArray = tempArray / s1%n

        call polygonCenter3%moveTo (tempArray)

    end function

    type(point2) function polygonCenter4 (s1)
        class(polyGon4), intent(in) :: s1

        real tempArray(2)

        tempArray = 0

        do i = 1, s1%n
            tempArray = tempArray + s1%vertices(i)%vertices()
        end do

        tempArray = tempArray / s1%n

        call polygonCenter4%moveTo (tempArray)

    end function

    subroutine movePolygon3 (s1, where)
        class (polyGon3), intent(inout) :: s1
        type(point2), intent(in) :: where

        associate (center => s1%center())
            do i = 1, s1%n
                call s1%vertices(i)%moveBy(where%vertices()-center%vertices())
            end do
        end associate
    end subroutine

    subroutine movePolygon4 (s1, where)
        class (polyGon4), intent(inout) :: s1
        type(point2), intent(in) :: where

        associate (center => s1%center())
            do i = 1, s1%n
                call s1%vertices(i)%moveBy(where%vertices()-center%vertices())
            end do
        end associate
    end subroutine
end module


use geometryMod
    implicit none
    type(polyGon3), allocatable :: pg1
    type(polyGon4), allocatable :: pg11
    type(point2), allocatable :: p1(:)

    type(point2) center, origin

    logical(4), external :: precision_r4
    real, dimension(2) :: r1
    real, parameter :: delta = 5.e-8

    integer i, noSize


    call origin%moveTo ([0.0, 0.0])

    !! test 1: test a triangle
    noSize = 3

    allocate (polyGon3 :: pg1)
    allocate (p1(noSize))

    call p1(1)%moveTo ([1.0, 1.0])
    call p1(2)%moveTo ([1.0, -1.0])
    call p1(3)%moveTo ([0.0, 1.0])

    ! setup this triangle
    pg1 = polyGon3(noSize,p1)

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

    pg11 = polyGon4(noSize,p1)

    call pg11%moveTo (origin)

    !! verify center
    associate (x => pg11%center())
        r1 = x%vertices()

        if (any(abs(r1) > delta)) error stop 5_4
    end associate

    !! verify new corrdinates of 4 vertices

    do i = 1, noSize
        associate (r2 => pg11%vertices(i))
            if (any(abs(r2%diff(p1(i)) + 0.5) > delta)) error stop 6_4
        end associate
    end do
end
