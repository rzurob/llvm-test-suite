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
!*  DATE                       : 10/19/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that poly-allocatable dummy-arg in the
!                               assignment.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        complex(8), allocatable :: cx(:)

        contains

        procedure, pass(b) :: print => printBase
    end type

    type, extends(base) :: child
        character(:), pointer :: names(:)

        contains

        procedure, pass(b) :: print => printChild
    end type

    contains

    integer function printBaseCxAllocStatus (unit, b)
        class(base), intent(in) :: b
        integer, intent(in) :: unit

        allocatable printBaseCxAllocStatus

        if (allocated(b%cx)) then
            write (unit, *) 'cx bounds: ', lbound(b%cx), ubound(b%cx)

            printBaseCxAllocStatus = 1
        else
            write (unit, *) 'cx not allocated'

            printBaseCxAllocStatus = 0
        end if
    end function

    subroutine printBase (unit, b, fmt)
        class(base), intent(in) :: b
        integer, intent(in) :: unit
        character(*), intent(in) :: fmt

        if (printBaseCxAllocStatus(unit,b) == 1)  write (unit, fmt) b%cx
    end subroutine

    subroutine printChild (unit, b, fmt)
        class(child), intent(in) :: b
        integer, intent(in) :: unit
        character(*), intent(in) :: fmt

        if (printBaseCxAllocStatus(unit,b) == 1) then
            if (associated(b%names)) then
                write (unit, *) 'names bounds', lbound(b%names), ubound(b%names)
                write (unit, fmt) b%cx, b%names
            else
                write (unit, fmt) b%cx, 'NULL'
            end if
        end if
    end subroutine
end module


program dummyArg008a
use m
use, intrinsic :: iso_fortran_env, only : output_unit
    interface assgn
        subroutine assgnBase (b1, b2)
        import base
            class(base), allocatable, intent(out) :: b1
            class(base) b2
        end subroutine
    end interface

    type container
        class(base), allocatable :: data
    end type

    type(container), allocatable :: co1, co2

    character(20), target :: str(10)

    str = 'test'

    co1 = container(null())

    co2 = container(child([(cmplx(i,i-1), i=1,10)], str))

    call assgn (co1%data, co2%data)

    call co1%data%print(output_unit, '(10("(",f15.6, f15.6,") "), 10a)')

    call assgn (co1%data, base([(1.2d0, 2.2d0), (3.2d0, 4.2d0)]))

    call co1%data%print(output_unit, '(4f12.4)')
end


subroutine assgnBase (b1, b2)
use m
    class(base), allocatable, intent(out) :: b1
    class(base) b2

    if (same_type_as (b2, base(null()))) then
        allocate(b1)
    else if (same_type_as (b2, child(null(), null()))) then
        allocate (child:: b1)
    else
        stop 10
    end if

    select type (b1)
        type is (base)
            b1 = b2

        type is (child)
            select type (b2)
                type is (child)
                    b1 = b2

                class default
                    stop 15
            end select

        class default
            stop 11

    end select
end subroutine
