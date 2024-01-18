! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/31/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound procedure
!                               A test case on geometry and different shapes.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module pointMod
    type point (n)
        integer, len :: n

        private

        real :: coord(n)

        contains

        procedure :: moveTo
        procedure :: moveBy
        procedure :: vertices
        procedure :: diff => pointDiff
    end type

    contains

    subroutine moveTo (p1, r1)
        class(point(*)), intent(inout) :: p1
        real, intent(in) :: r1(p1%n)

        p1%coord = r1
    end subroutine

    subroutine moveBy (p1, r1)
        class(point(*)), intent(inout) :: p1
        real, intent(in) :: r1(p1%n)

        call p1%moveTo (r1+p1%vertices())
    end subroutine

    function vertices (p1)
        class(point(*)), intent(in) :: p1

        real, dimension(p1%n) :: vertices

        vertices = p1%coord
    end function

    function pointDiff (p1, p2)
        class(point(*)), intent(in) :: p1, p2

        real, dimension(p1%n) :: pointDiff

        if (p1%n /= p2%n) error stop 10

        pointDiff = p1%vertices() - p2%vertices()
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
        type(point(2)) function absInf1 (s1)
            import
            class(shape), intent(in) :: s1
        end function

        subroutine absInf2 (s1, where)
            import
            class(shape), intent(inout) :: s1
            type(point(2)), intent(in) :: where
        end subroutine
    end interface
end module


module geometryMod
use shapeMod
    type, extends(shape) :: polyGon (n)
        integer, len :: n

        type(point(2)) :: vertices(n)

        contains

        procedure :: center => polygonCenter
        procedure :: moveTo => movePolygon
    end type

    contains

    type(point(2)) function polygonCenter (s1)
        class(polyGon(*)), intent(in) :: s1

        real tempArray(2)

        tempArray = 0

        do i = 1, s1%n
            tempArray = tempArray + s1%vertices(i)%vertices()
        end do

        tempArray = tempArray / s1%n

        call polygonCenter%moveTo (tempArray)
    end function

    subroutine movePolygon (s1, where)
        class (polyGon(*)), intent(inout) :: s1
        type(point(2)), intent(in) :: where

        associate (center => s1%center())
            do i = 1, s1%n
                call s1%vertices(i)%moveBy(where%vertices()-center%vertices())
            end do
        end associate
    end subroutine
end module

