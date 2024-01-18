!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/28/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 316767)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    integer, parameter :: numberOfElement = 50000

    type base4_53! (k, n)
        real(4) :: data(53) = sqrt(-1.0)
    end type

    type, extends(base4_53) :: child4_53
        integer(4) :: id(53) = 4
    end type

    type base8_50000! (k, n)
        real(8) :: data(numberOfElement) = sqrt(-1.0)
    end type

    type, extends(base8_50000) :: child8_50000
        integer(8) :: id(numberOfElement) = 8
    end type

    type container4_53!(k, n)
        type(child4_53) :: data = child4_53((/(i, i=1,53)/),(/(i, i=1,53)/))
    end type

    type container8_50000!(k, n)
        type(child8_50000) :: data = child8_50000((/(i, i=1,numberOfElement)/),(/(i, i=1,numberOfElement)/))
    end type

    type (container8_50000) :: co1 = container8_50000()
end module

program dtparamDefInit002
use ieee_arithmetic
use m
    type (container4_53) :: co2(10) = container4_53(child4_53())

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
