! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/30/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.5: comp. order)
!                               Case: Test component order in structure
!                               constructor; use a class hirarchy of 3
!                               generations.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: id
    end type

    type, extends(base) :: child (n)
        integer, len :: n

        real :: data (n)
    end type

    type, extends(child) :: gen3 (k)
        integer, kind :: k

        complex(k) :: cx(n)
    end type

    type (child(10)) :: c1_m = child(10)(1, (/(i*1.0, i=1, 10)/))
    type (gen3(8, 8)) :: g1_m = gen3(8,8) (10, (/(i*1.2, i=0, 7)/), &
            (/((i*3.1d0, i+3.1d0), i=1, 8)/))
end module

program dtparamCompOrder002
use m
    class (base), allocatable :: b1

    logical(4), external :: precision_r4, precision_x6, precision_x8

    allocate (b1, source=gen3(20, 4)(100, (/(i+2.1e1, i=0,19)/), &
            (/(cmplx(i, kind=8), i = 1,20)/)))

    !! verify the results
    if ((c1_m%id /= 1) .or. (g1_m%id /= 10)) error stop 1_4

    do i = 1, 10
        if (.not. precision_r4(c1_m%data(i), i*1.0)) error stop 2_4

        if (i >= 9) cycle

        if (.not. precision_r4(g1_m%data(i), (i-1)*1.2)) error stop 3_4
        if (.not. precision_x6(g1_m%cx(i), (i*3.1d0, i+3.1d0))) error stop 4_4
    end do

    if (b1%id /= 100) error stop 5_4

    select type (b1)
        type is (gen3(*,4))
            do i = 1, 20
                if (.not. precision_r4 (i-1+2.1e1, b1%data(i))) error stop 6_4

                if (.not. precision_x8 (b1%cx(i), (i*1.0, 0.0))) error stop 7_4
            end do

        class default
            error stop 10_4
    end select
end
