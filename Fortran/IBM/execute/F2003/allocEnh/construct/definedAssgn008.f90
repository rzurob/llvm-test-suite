!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/31/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               More test on that the defined assignment is
!                               invoked at extended type level; the derived type
!                               with unlimited poly-allocatable component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real x(2)

        contains

        procedure :: assgnPoint
        generic :: assignment(=) => assgnPoint
    end type

    type, extends(point) :: uPoint
        contains

        procedure :: assgnPoint => assgnUpoint
        generic :: assignment(=) => assgnPoint
    end type

    type line
        class(point), allocatable :: points(:)
    end type

    contains

    elemental subroutine assgnPoint (u1, u2)
        class(point), intent(out) :: u1
        class(point), intent(in) :: u2

        u1%x = u2%x + 1.0
    end subroutine

    elemental subroutine assgnUpoint (u1, u2)
        class(uPoint), intent(out) :: u1
        class(point), intent(in) :: u2

        real radius

        radius = sqrt(u2%x(1)**2 + u2%x(2)**2)

        if (radius < 1.0e-5) then
            u1%x = sqrt(-1.0)
        else
            u1%x = u2%x/radius
        end if
    end subroutine
end module

module m1
    type container
        class(*), allocatable :: data
    end type
end module

program definedAssgn008
use m
use m1
    type(container), allocatable :: co1
    type(container) co2

    logical(4), external :: precision_r4

    allocate (co2%data, source=line([uPoint([1.0, 2.0]), uPoint([3.0,4.0])]))

    co1 = co2

    select type (x => co1%data)
        class is (line)
            select type (y => x%points)
                class is (point)
                    if (.not. precision_r4(y(1)%x(1), 0.4472135901_4)) &
                        error stop 1_4

                    if (.not. precision_r4(y(1)%x(2), 0.8944271803_4)) &
                        error stop 2_4

                    if (.not. precision_r4(y(2)%x(1), 0.6_4)) &
                        error stop 3_4

                    if (.not. precision_r4(y(2)%x(2), 0.8_4)) &
                        error stop 4_4

                class default
                    stop 20
            end select

        class default
            stop 10

    end select
end
