! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/12/2007
!*
!*  DESCRIPTION                : derived type parameter (component)
!                               This test case tests the initialization of
!                               component that is of derived type without type
!                               parameter.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: id(10)
    end type

    type collector (num)
        integer, len :: num

        type (base) :: data(num)! = base((/(i, i=num,num+9)/))
    end type

    interface collector
        module procedure createCollector
    end interface

    contains

    function createCollector (n)
        integer, intent(in) :: n
        type(collector(n)) createCollector

        createCollector%data = base((/(i, i=n,n+9)/))
    end function
end module

program dtparamCompInit010
use m
    type (collector(20)) :: co1
    class(collector(300)), allocatable :: co2(:)


    co1 = collector(20)

    allocate (co2(30), source=collector(300))

    !! verify initialization
    do i = 1, 20
        do j = 1, 10
            if (co1%data(i)%id(j) /= 20+j-1) error stop 1_4
        end do
    end do

    do i = 1, 30
        do j = 1, 300
            do k = 1, 10
                if (co2(i)%data(j)%id(k) /= 299+k) error stop 2_4
            end do
        end do
    end do
end
