! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/25/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               An extension of dtparamConstr056a: use function
!                               results to supply the data for the unlimited
!                               poly component; still scalar.
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

        procedure :: print8 => printPoint8
    end type

    type, abstract :: shape (k)
        integer, kind :: k

        contains

        procedure(getShape8), deferred :: getShape8
    end type


    abstract interface
        function getShape8 (s)
        import
            class(shape(8)), intent(in) :: s
            class(shape(8)), allocatable :: getShape8
        end function
    end interface

    type, extends(shape) :: circle
        type(point(k)) :: center
        real(k) :: radius

        contains

        procedure :: getShape8 => getCircle8
    end type

    contains

    subroutine printPoint8 (p, unit, fmt)
        class(point(8)), intent(in) :: p
        integer, intent(in) :: unit
        character(*), intent(in) :: fmt

        write (*,fmt) p%x, p%y
    end subroutine

    class(shape(8)) function getCircle8 (s)
        class(circle(8)), intent(in) :: s

        allocatable getCircle8

        allocate (getCircle8, source=s)
    end function
end module

program dtparamConstr056a1
use m
use n
use iso_fortran_env
    class(shape(8)), allocatable :: s1

    type(base) b1

    allocate (s1, source=circle(8)(point(8)(1.1d0, 3.2d-1), 12.2d0))

    b1 = base(s1%getShape8())

    if (.not. allocated(b1%data)) error stop 1_4

    if (.not. same_type_as (b1%data, circle(8)(point(8)(0,0), 0))) &
        error stop 2_4

    select type (x => b1%data)
        class is (circle(8))
            call x%center%print8 (OUTPUT_UNIT, '(2f15.6)')
            write (*, '(a, f15.6)') 'radius=', x%radius

        class default
            error stop 1_4
    end select
end
