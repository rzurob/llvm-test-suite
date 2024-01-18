!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/31/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Further test that the defined type-bound
!                               assignment is invoked during the intrinsic
!                               assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, pointer :: data

        contains

        final :: finalizeBase
        procedure :: assgnB1B2
        generic :: assignment(=) => assgnB1B2
    end type

    contains

    elemental subroutine finalizeBase (b)
        type(base), intent(inout) :: b

        if (associated(b%data)) deallocate(b%data)
    end subroutine

    elemental subroutine assgnB1B2 (b1, b2)
        class(base), intent(inout) :: b1
        type(base), intent(in) :: b2

        if (associated(b2%data)) then
            allocate(b1%data)
            b1%data = b2%data + 1
        end if
    end subroutine
end module

module m1
use m
    type container
        class(base), allocatable :: data
    end type
end module

program dummyArg013
use m1
    type(container), allocatable :: co1(:)

    type(container) :: co2(0:29)

    logical(4), external :: precision_r4

    do i = 0, 29
        allocate (co2(i)%data)
        allocate (co2(i)%data%data)

        co2(i)%data%data = log(i+1.5)
    end do

    co1 = co2

    !! verify  co1 and co2
    if ((lbound(co1,1) /= 0) .or. (ubound(co1,1) /= 29)) error stop 1_4

    do i = 0, 29
        if ((.not. allocated(co1(i)%data)) .or. &
            (.not. associated(co1(i)%data%data))) error stop 2_4

        if ((.not. allocated(co2(i)%data)) .or. &
            (.not. associated(co2(i)%data%data))) error stop 3_4

        if (.not. precision_r4 (co1(i)%data%data, 1 + log(i+1.5))) &
            error stop 4_4

        if (.not. precision_r4 (co2(i)%data%data, log(i+1.5))) error stop 5_4
    end do

    !! 2nd test
    co1 = [co2, co1, co2]

    if ((lbound(co1,1) /= 1) .or. (ubound(co1,1) /= 90)) error stop 6_4

    do i = 0, 29
        if ((.not. allocated(co1(i+1)%data)) .or. &
            (.not. associated(co1(i+1)%data%data))) error stop 7_4

        if (.not. precision_r4(co1(i+1)%data%data, 1 + log(i+1.5))) &
            error stop 8_4

        if ((.not. allocated(co1(31+i)%data)) .or. &
            (.not. associated(co1(i+31)%data%data))) error stop 9_4

        if (.not. precision_r4(co1(i+31)%data%data, 2+log(i+1.5))) &
            error stop 10_4

        if ((.not. allocated(co1(61+i)%data)) .or. &
            (.not. associated(co1(i+61)%data%data))) error stop 11_4

        if (.not. precision_r4(co1(i+61)%data%data, 1 + log(i+1.5))) &
            error stop 12_4
    end do
end
