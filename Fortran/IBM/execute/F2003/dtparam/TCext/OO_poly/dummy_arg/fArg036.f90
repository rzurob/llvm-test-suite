! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg036.f
! SCCS ID Information
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
    type, abstract :: shape(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure(printShape), deferred :: draw
        procedure(moveAlongX), deferred :: moveX
        procedure(moveAlongY), deferred :: moveY
        procedure(shapeArea), deferred :: area
    end type

    type point(k2)    ! (4)
        integer, kind :: k2
        real(k2)         x, y
    end type

    interface
        real function shapeArea (s)
        import shape
            class (shape(4,*)), intent(in) :: s
        end function
    end interface


    interface
        subroutine printShape (s)
        import shape
            class(shape(4,*)), intent(in) :: s
        end subroutine

        pure subroutine moveAlongX (s, dx)
        import shape
            class(shape(4,*)), intent(inout) :: s
            real, intent(in) :: dx
        end subroutine

        pure subroutine moveAlongY (s, dy)
        import shape
            class(shape(4,*)), intent(inout) :: s
            real, intent(in) :: dy
        end subroutine
    end interface


    contains

    elemental subroutine shapeMove (s, x, y)
        class(shape(4,*)), intent(inout) :: s
        real, intent(in) :: x, y

        call s%moveX(x)
        call s%moveY(y)
    end subroutine
end module

module m1
use m
    type, extends (shape) :: circle    ! (4,20)
        type (point(k1)) :: center
        real(k1) radius

        contains

        procedure :: draw => printCircle
        procedure :: moveX => moveCircleAlongX
        procedure :: moveY => moveCircleAlongY
        procedure :: area => circleArea
    end type

    contains

    real function circleArea (s)
        class(circle(4,*)), intent(in) :: s

        real(8), parameter :: pi = 3.14159265358_8

        circleArea = pi * s%radius * s%radius
    end function

    subroutine printCircle (s)
        class (circle(4,*)), intent(in) :: s

        write (*, '(a,2f10.2)') 'circle center:', s%center%x, s%center%y
        write (*, '(a, f10.2)') 'circle radius:', s%radius
    end subroutine

    pure subroutine moveCircleAlongX (s, dx)
        class (circle(4,*)), intent(inout) :: s
        real, intent(in) :: dx

        s%center%x = s%center%x + dx
    end subroutine

    pure subroutine moveCircleAlongY (s, dy)
        class(circle(4,*)), intent(inout) :: s
        real, intent(in) :: dy

        s%center%y = s%center%y + dy
    end subroutine
end module

program fArg036
use m1
    real(8), parameter :: pi = 3.14159265358_8
    class (shape(4,:)), allocatable :: s1, s2(:)
    logical precision_r4

    !! 1st test
    allocate (s1, source=circle(4,20)(point(4)(1.0, 1.0), 1.5))

    if (.not. precision_r4(s1%area(), 7.068583_4)) error stop 1_4

    call shapeMove (s1, -1.0, 2.0)

    call s1%draw

    if (.not. precision_r4(s1%area(), 7.068583_4)) error stop 2_4

    !! 2nd test
    print *, 'test 2'

    allocate(s2(10), source=(/(circle(4,20)(point(4)(0.,0.), radius=i*1.0), i=1,10)/))

    call shapeMove (s2, (/(i*1.0, i=1,10)/), 0.)

    do i = 1, 10
        call s2(i)%draw()

        if (.not. precision_r4 (s2(i)%area(), real(pi*i*i, 4))) error stop 3_4
    end do
end
