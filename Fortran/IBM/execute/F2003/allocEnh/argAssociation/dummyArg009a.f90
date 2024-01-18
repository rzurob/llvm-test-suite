!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               This is a simple test to verify that type-bound
!                               assignment applies for a derived type component
!                               that is nonpointer, nonallocatable and with a
!                               type-bound defined assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real :: data

        contains

        procedure :: uncommonAssgn
        generic :: assignment(=) => uncommonAssgn
    end type

    contains

    subroutine uncommonAssgn (b1, b2)
        class(base), intent(inout) :: b1
        type(base), intent(in) :: b2

        b1%data = b1%data + b2%data
    end subroutine
end module

module m1
use m
    type container
        type(base) data
    end type
end module

program dummyArg009a
use m1
    type(container) :: co1, co4
    logical(4), external :: precision_r4

    co1%data%data = 10.1
    co4%data%data = 20.2

    co1 = co4

    if (.not. precision_r4 (co1%data%data, 10.1_4+20.2_4)) error stop 1_4
    if (.not. precision_r4 (co4%data%data, 20.2_4)) error stop 2_4
end
