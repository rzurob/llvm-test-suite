!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/20/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that associate name's allocatable component
!                               appear as the variable in an intrinsic
!                               assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, allocatable :: data(:)

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        type(base), allocatable :: r1

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b

        if (allocated(b%data)) then
            print *, lbound(b%data), 'to', ubound(b%data)

            write (*, '(12g10.2)') b%data
        end if
    end subroutine

    subroutine printChild (b)
        class(child), intent(in) :: b

        call b%base%print

        if (allocated(b%r1)) then
            print *, 'child type component r1:'

            call b%r1%print
        end if
    end subroutine
end module

program selectType003
use m
    class(base), allocatable :: b1

    allocate(child :: b1)

    select type (x => b1)
        type is (child)
            x%data = (/(j, j=1,100)/)
            x%r1 = base(x%data)

        class default
            stop 10
    end select

    call b1%print
end
