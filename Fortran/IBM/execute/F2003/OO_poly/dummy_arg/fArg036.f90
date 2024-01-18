!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/17/2005
!*
!*  DESCRIPTION                : argument association (abstract type as the
!                               dummy-arg)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
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

module m1
use m
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

program fArg036
use m1
    real(8), parameter :: pi = 3.14159265358_8
    class (shape), allocatable :: s1, s2(:)
    logical precision_r4

    !! 1st test
    allocate (s1, source=circle(point(1.0, 1.0), 1.5))

    if (.not. precision_r4(s1%area(), 7.068583_4)) error stop 1_4

    call shapeMove (s1, -1.0, 2.0)

    call s1%draw

    if (.not. precision_r4(s1%area(), 7.068583_4)) error stop 2_4

    !! 2nd test
    print *, 'test 2'

    allocate(s2(10), source=(/(circle(point(0.,0.), radius=i*1.0), i=1,10)/))

    call shapeMove (s2, (/(i*1.0, i=1,10)/), 0.)

    do i = 1, 10
        call s2(i)%draw()

        if (.not. precision_r4 (s2(i)%area(), real(pi*i*i, 4))) error stop 3_4
    end do
end
