! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2006
!*
!*  DESCRIPTION                : dtparam (section 4.7: array constructor)
!                               Use of type parameter inquiry in the
!                               ac-implied-do loop.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data (n)
    end type
end module

program dtparamArrConstr004
use m
    type(base(4, 33)) :: b1
    type(base(8, :)), pointer :: b2(:)

    logical(4), external :: precision_r4, precision_r8

    b1 = base(4, 33)((/(j*1.0e0, j=1, b1%n)/))

    allocate (base(8, 54) :: b2(10))

    do i = 1, 10
        b2(i)%data = log((/(i*1.0d2+j, j = 1, b2%n)/))
    end do

    !! verify
    do i = 1, b1%n
        if (.not. precision_r4(b1%data(i), i*1.0e0)) error stop 1_4
    end do

    do i = 1, size(b2)
        do j = 1, b2(i)%n
            if (.not. precision_r8(b2(i)%data(j), log(i*1.0d2+j))) &
                    error stop 2_4
        end do
    end do
end
