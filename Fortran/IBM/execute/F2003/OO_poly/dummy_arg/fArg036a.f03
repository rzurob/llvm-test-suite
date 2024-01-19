! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2005
!*
!*  DESCRIPTION                : argument association (abstract type as the
!                               dummy-arg)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module shapeMod
    type, abstract :: shape
        contains

        procedure(printShape), deferred :: draw
        procedure(moveAlongX), deferred :: moveX
        procedure(moveAlongY), deferred :: moveY
        procedure(shapeArea), deferred :: area
    end type

    type point
        real x, y
    end type

    interface
        real function shapeArea (s)
        import shape
            class (shape), intent(in) :: s
        end function
    end interface


    interface
        subroutine printShape (s)
        import shape
            class(shape), intent(in) :: s
        end subroutine

        pure subroutine moveAlongX (s, dx)
        import shape
            class(shape), intent(inout) :: s
            real, intent(in) :: dx
        end subroutine

        pure subroutine moveAlongY (s, dy)
        import shape
            class(shape), intent(inout) :: s
            real, intent(in) :: dy
        end subroutine
    end interface


    contains

    elemental subroutine shapeMove (s, x, y)
        class(shape), intent(inout) :: s
        real, intent(in) :: x, y

        call s%moveX(x)
        call s%moveY(y)
    end subroutine
end module

module circleMod
use shapeMod
    type, extends (shape) :: circle
        type (point) :: center
        real radius

        contains

        procedure :: draw => printCircle
        procedure :: moveX => moveCircleAlongX
        procedure :: moveY => moveCircleAlongY
        procedure :: area => circleArea
    end type

    contains

    real function circleArea (s)
        class(circle), intent(in) :: s

        real(8), parameter :: pi = 3.14159265358_8

        circleArea = pi * s%radius * s%radius
    end function

    subroutine printCircle (s)
        class (circle), intent(in) :: s

        write (*, '(a,2f10.2)') 'circle center:', s%center%x, s%center%y
        write (*, '(a, f10.2)') 'circle radius:', s%radius
    end subroutine

    pure subroutine moveCircleAlongX (s, dx)
        class (circle), intent(inout) :: s
        real, intent(in) :: dx

        s%center%x = s%center%x + dx
    end subroutine

    pure subroutine moveCircleAlongY (s, dy)
        class(circle), intent(inout) :: s
        real, intent(in) :: dy

        s%center%y = s%center%y + dy
    end subroutine
end module

module rectangleMod
use shapeMod
    type, extends(shape) :: rectangle
        real width, height
        real x, y

        contains

        procedure :: draw => printRectangle
        procedure :: moveX => moveRectAlongX
        procedure :: moveY => moveRectAlongY
        procedure :: area => rectangleArea
    end type

    type, extends(rectangle) :: square
    end type

    interface square
        module procedure makeSquare
    end interface

    contains

    real function rectangleArea (s)
        class(rectangle), intent(in) :: s

        rectangleArea = s%width * s%height
    end function

    pure subroutine moveRectAlongX (s, dx)
        class(rectangle), intent(inout) :: s
        real, intent(in) :: dx

        s%x = s%x + dx
    end subroutine

    pure subroutine moveRectAlongY (s, dy)
        class(rectangle), intent(inout) :: s
        real, intent(in) :: dy

        s%y = s%y + dy
    end subroutine

    subroutine printRectangle (s)
        class (rectangle), intent(in) :: s

        write (*, '(1x,a,2f10.2)') 'top left point:', s%x, s%y
        write (*, '(1x,a,f10.2,a,f10.2)') 'width =', s%width, ', height =', &
                s%height
    end subroutine

    type (square) function makeSquare (p1, side)
        type(point), intent(in) :: p1
        intent(in) :: side

        makeSquare%x = p1%x
        makeSquare%y = p1%y
        makeSquare%width = side
        makeSquare%height = side
    end function
end module

module geometry
use circleMod
use rectangleMod

    contains

    subroutine createShape (s, s1)
        class(shape), intent(out), allocatable :: s
        class(shape), intent(in) :: s1

        allocate (s, source=s1)
    end subroutine

    subroutine createShapes (s, s1)
        class(shape), intent(out), allocatable :: s(:)
        class(shape), intent(in) :: s1(:)

        allocate (s(size(s1)), source=s1)
    end subroutine
end module

program fArg036a
use geometry

    class(shape), allocatable :: s1, s2(:)

    !! test 1
    call createShape (s1, s1=square(point(10.0, 0.0), 10.0))

    call s1%draw

    write (*, '(1x,a,f12.2)') 'square area = ', s1%area()


    !! test 2
    call createShape (s1, circle(center=point(1.0, 1.0), radius=2.0))

    call s1%draw

    write (*, '(1x,a,f12.2)') 'circle area = ', s1%area()


    !! test 3
    call createShapes (s2, s1 = (/s1, s1/))

    if (size(s2) /= 2) error stop 1_4

    call s2(1)%draw
    call s2(2)%draw()
end
