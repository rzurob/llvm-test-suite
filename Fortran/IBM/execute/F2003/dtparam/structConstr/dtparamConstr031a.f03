! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/10/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Data conversion between numerical types.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        integer(k) :: id
        real(k) :: data (n)
        complex(k) :: cx
    end type

    real(8), parameter :: pi = 3.14159265359d0
    complex(8), parameter :: cmpx = (10*pi, pi)
end module

program dtparamConstr031
use m
    type (base(8,20)) :: b1

    logical(4), external :: precision_r8, precision_x6

    b1 = base(8,20)(10.0*sqrt(cmpx), cmpx, pi)

    if (b1%id /= 56) error stop 1_4

    do i = 1, 20
        if (.not. precision_r8 (b1%data(i), 1.0d1*pi)) error stop 2_4
    end do

    if (.not. precision_x6 (b1%cx, cmplx(pi, 0, kind =8))) error stop 3_4
end
