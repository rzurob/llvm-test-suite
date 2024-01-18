!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.4: default init.)
!                               Case: Use of scalar for the default
!                               initialization for the array component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        integer :: id(n)
    end type

    type collector (num)
        integer, kind :: num

        type (base(num)) :: data(num) = base(num)((/(i, i=1,num)/))
    end type
end module

program dtparamCompInit002
use m
    type (collector(10)) :: co1
    class(collector(20)), allocatable :: co2

    allocate (co2)

    do i = 1, 10
        if (any(co1%data(i)%id /= (/(j, j = 1, 10)/))) error stop 1_4
    end do

    do i = 1, 20
        do j = 1, 20
            if (co2%data(i)%id(j) /= j) error stop 2_4
        end do
    end do
end
