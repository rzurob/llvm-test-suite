! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/27/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               miscellaneous (defect 327270); a 2nd variance.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, pointer :: data
    end type
end module

module m1
use m
    type container
        type(base), allocatable :: data
    end type
end module

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

    co1 = [co2, co1, co2]

    do i = 1, 30
        if (.not. associated(co1(i)%data%data, co2(i-1)%data%data)) &
            error stop 2_4

        if (.not. precision_r4 (co1(i)%data%data, log(i+0.5))) error stop 10_4

        if (.not. associated(co1(30+i)%data%data, co2(i-1)%data%data)) &
            error stop 3_4


        if (.not. associated(co1(60+i)%data%data, co2(i-1)%data%data)) &
            error stop 3_4
    end do
end