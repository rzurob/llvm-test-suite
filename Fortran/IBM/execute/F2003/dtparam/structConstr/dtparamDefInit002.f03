! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/28/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Structure constructor for derived type
!                               with default initializations; component is of
!                               parameterized derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n) = sqrt(-1.0)
    end type

    type, extends(base) :: child
        integer(k) :: id(n) = k
    end type

    type container(k, n)
        integer, kind :: k, n

        type(child(k,n)) :: data = child(k,n)((/(i, i=1,n)/),(/(i, i=1,n)/))
    end type

    integer, parameter :: numberOfElement = 50000

    type (container(8, numberOfElement)) :: co1 = container(8,numberOfElement)()
end module

program dtparamDefInit002
use ieee_arithmetic
use m
    type (container(4, 53)) :: co2(10) = container(4,53)(child(4,53)())

    logical(4), external :: precision_r8

    !! verify the components' initializations

    do i = 1, numberOfElement
        if (.not. precision_r8(co1%data%data(i), real(i, 8))) error stop 1_4

        if (co1%data%id(i) /= i) error stop 2_4
    end do

    do i = 1, 10
        do j = 1, 53
            if (.not. ieee_is_nan(co2(i)%data%data(j))) error stop 3_4

            if (co2(i)%data%id(j) /= 4) error stop 4_4
        end do
    end do
end
