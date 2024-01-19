! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/07/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Use of NaNQ and INF for derived type
!                               components' initializations.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k, n1, n2)
        integer, kind :: k
        integer, len :: n1, n2

        real(k) :: data(n1, n2) = sqrt(-1.0) !<-- NaNQ
        real(k) :: data2(n1+n2) = 1/sqrt(0.0) !<-- INF
    end type

    type (A(8, 10, 10)), save :: a1
end module

program compInit001
use ieee_arithmetic
use m
    type (A(4, 5, 15)), allocatable :: a2
    type (A(16, :,:)), pointer :: a3(:)

    allocate (a2)
    allocate (A(16, 200, 12) :: a3(10))

    do i = 1, 10
        do j = 1, 10
            if (.not. ieee_is_nan(a1%data(i,j))) error stop 1_4
        end do
    end do

    do i = 1, 20
        if (ieee_is_finite(a1%data2(i))) error stop 2_4
    end do

    do i = 1, 5
        do j = 1, 15
            if (.not. ieee_is_nan(a2%data(i, j))) error stop 3_4
        end do
    end do

    do i = 1, 20
        if (ieee_is_finite(a2%data2(i))) error stop 4_4
    end do

    do i =1, 10
        do j = 1, 200
            do k = 1, 12
                if (.not. ieee_is_nan(a3(i)%data(j,k))) error stop 5_4
            end do
        end do

        do j = 1, 212
            if (ieee_is_finite(a3(i)%data2(j))) error stop 6_4
        end do
    end do
end
