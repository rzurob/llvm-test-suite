! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/09/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Use of integer(8) for the bounds of the
!                               array components; also use procedures to update
!                               the derived type values.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (lb, ub)
        integer(8), len :: lb, ub

        real(selected_int_kind(12)) :: data(lb:ub)
    end type

    contains

    subroutine updateVal (b, d1)
        type (base(*,*)), intent(out) :: b
        real(8), intent(in) :: d1(b%lb:b%ub)

        b%data = d1
    end subroutine
end module

program dtparamValConvert004
use ieee_arithmetic
use m
    logical(4), external :: precision_r8

    type (base(lb=-10_1, ub=1300)) b1

    type (base(lb=:, ub=:)), allocatable :: b2(:)

    double precision d1(1400)

    d1 = log((/(i*1.0d0, i=-10,1389)/))

    allocate(base(lb=-100_8, ub=3000_2):: b2(10))

    call updateVal (b1, d1)

    do i = 1, 10
        call updateVal (b2(i), log((/(i*1.0d4+j, j=-100, 3000)/)))
    end do

    !! verify
    do i = -10, -1
        if (.not. ieee_is_nan(b1%data(i))) error stop 1_4
    end do

    if (ieee_is_finite(b1%data(0))) error stop 2_4

    do i = 1, 1300
        if (.not. precision_r8(b1%data(i), log(i*1.0d0))) error stop 3_4
    end do

    do i = 1, 10
        do j = -100, 3000
            if (.not. precision_r8(b2(i)%data(j), log(i*1.0d4+j))) &
                    error stop 4_4
        end do
    end do
end
