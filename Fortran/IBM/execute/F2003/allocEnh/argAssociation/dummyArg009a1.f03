! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Simple case tests for intrinsic assignment of a
!                               derived type with allocatable component that is
!                               of a type consistent with a type bound defined
!                               assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real :: data = -1

        contains

        procedure :: uncommonAssgn
        generic :: assignment(=) => uncommonAssgn
    end type

    type, extends(base) :: child
        character(10) :: name = 'XLF'

        contains

        procedure :: uncommonAssgn => doesNothing
    end type

    contains

    subroutine uncommonAssgn (b1, b2)
        class(base), intent(inout) :: b1
        class(base), intent(in) :: b2
    end subroutine

    subroutine doesNothing (b1, b2)
        class(child), intent(inout) :: b1
        class(base), intent(in) :: b2
    end subroutine
end module

module m1
use m
    type container
        class(base), allocatable :: data
    end type
end module

program dummyArg009
use m1
    type(container) :: co1, co4, co2, co3
    logical(4), external :: precision_r4

    allocate (co1%data, source = base(10.1))
    allocate (co4%data, source = base(20.2))
    allocate (co2%data, source = child(-100, 'ABC'))

    !! test 1
    co1 = co4

    if (.not. allocated(co1%data)) error stop 2_4

    if (.not. same_type_as(co1%data, base(1))) error stop 1_4

    if (.not. precision_r4 (co1%data%data, -1.0_4)) error stop 3_4

    !! test 2
    co1 = co2

    if (.not. allocated(co1%data)) error stop 4_4

    select type (x => co1%data)
        type is (child)
            if (.not. precision_r4 (x%data, -1.0_4)) error stop 5_4

            if (x%name /= 'XLF') error stop 6_4

        class default
            error stop 7_4
    end select

    !! test 3
    co1 = co3

    if (allocated(co1%data)) error stop 8_4
end
