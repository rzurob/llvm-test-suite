! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/23/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Expression used in the component data
!                               source; use intrinsic types.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type
end module

program dtparamConstr007
use m
    type (base(4, :)), allocatable :: b1
    type (base(8, 43)) :: b2

    logical(4), external :: precision_r4, precision_r8

    allocate (base(4,77) ::b1)

    b1 = base(4, 77)((/(i*1.0, i=1,77)/))
    b2 = base(8, 43)((/(i*1.21, i=1,43)/))

    !! verify
    if (b1%n /= 77) error stop 10_4

    do i = 1, 77
        if (.not. precision_r4(i*1.0, b1%data(i))) error stop 1_4
    end do

    do i = 1, 43
        if (.not. precision_r4(real(b2%data(i), 4), &
            real(real(i*1.21, 8), 4))) error stop 2_4
    end do
end
