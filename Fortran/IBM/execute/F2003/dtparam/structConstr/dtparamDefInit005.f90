! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/14/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of the named constants for the default
!                               initializations.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    real(4), parameter :: r_const (30000) = sqrt((/(i*1.0, i=1,30000)/))

    type base (k, n)
        integer, kind :: k
        integer(selected_int_kind(4)), kind :: n

        real(k) :: data(n) = r_const(k:n+k-1)
    end type

    type(base(4, 3000)) :: b1 = base(4,3000)()
end module

program dtparamDefInit005
use m
    class(base(8,8000)), allocatable :: b2(:)

    logical(4), external :: precision_r4

    allocate (b2(10), source=base(8,8000)())

    !! verify
    do i = 1, 3000
        if (.not. precision_r4 (b1%data(i), sqrt(3.0+i))) error stop 1_4
    end do

    do i = 1, 10
        do j = 1, 8000
            if (.not. precision_r4 (real(b2(i)%data(j), 4), sqrt(7.0+j))) &
                error stop 2_4
        end do
    end do
end
