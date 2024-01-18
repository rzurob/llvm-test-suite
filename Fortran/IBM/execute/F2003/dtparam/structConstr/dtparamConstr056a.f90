!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/24/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Use the unlimted poly allocatable component;
!                               scalar component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), allocatable :: data
    end type
end module

module n
    type point (k)
        integer, kind :: k

        real(k) :: x, y

        contains

        procedure :: print4 => printPoint4
    end type

    type, abstract :: shape (k)
        integer, kind :: k

        contains

        procedure(getShapeCenter4), deferred :: getCenter4
    end type


    abstract interface
        type(point(4)) function getShapeCenter4 (s)
        import
            class(shape(4)), intent(in) :: s
        end function
    end interface

    type, extends(shape) :: circle
        type(point(k)) :: center
        real(k) :: radius

        contains

        procedure :: getCenter4 => getCircleCenter4
        procedure :: getRadius4 => getCircleRadius4
    end type

    contains

    subroutine printPoint4 (p, unit, fmt)
        class(point(4)), intent(in) :: p
        integer, intent(in) :: unit
        character(*), intent(in) :: fmt

        write (*,fmt) p%x, p%y
    end subroutine

    type(point(4)) function getCircleCenter4 (s)
        class(circle(4)), intent(in) :: s

        getCircleCenter4 = s%center
    end function

    real(4) function getCircleRadius4 (s)
        class(circle(4)), intent(in) :: s

        getCircleRadius4 = s%radius
    end function
end module

program dtparamConstr056a
use m
use n
use iso_fortran_env
    type (base) :: b1
    class(base), pointer :: b2

    b1 = base (circle(4)(center=point(4)(1.2, 3.1), radius=3.0))

    allocate (b2, source=base(circle(4)(point(4)(-1.5, sin(1.2)), 1.0)))

    !! verify b1 and b2
    if ((.not. allocated(b1%data)) .or. (.not. allocated(b2%data))) &
        error stop 1_4

    if (.not. same_type_as (b1%data, b2%data)) error stop 2_4

    select type (x => b1%data)
        class is (circle(4))
            associate (y => x%getCenter4())
                call y%print4(OUTPUT_UNIT, '(f10.3,1x,f10.3)')

                write (*,'(a,f10.3)') 'radius=', x%getRadius4()
            end associate

        class default
            error stop 3_4

    end select

    select type (x => b2%data)
        type is (circle(4))
            associate (y => x%getCenter4())
                call y%print4(OUTPUT_UNIT, '(f10.3,1x,f10.3)')
            end associate

            write (*,'(a,f10.3)') 'radius=', x%getRadius4()

        class default
            error stop 5_4

    end select
end
