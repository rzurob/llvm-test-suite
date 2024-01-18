!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/21/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: Kind type parameters are initialization
!                               expressions in the derived type definition; used
!                               in the default initializations.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (init)
        integer(8), kind :: init

        integer(8) :: id = init
    end type

    type, extends(base) :: child (val)
        integer(8), kind :: val

        real(4) :: data = val
        complex(4) :: cx = init
    end type
end module

program dtparamInitexpr003
use m

    logical (4) precision_r4, precision_x8
    external precision_r4, precision_x8

    type (base(2_8**33)) b1
    type (child (2_8**45, 2_8**40)) c1(2)

    !! verify the default initializations
    if (b1%id /= 8589934592_8) error stop 1_4

    if (any(c1%id /= 35184372088832_8)) error stop 2_4

    if (.not. precision_r4(c1(1)%data, 1.0995116e12))  error stop 3_4
    if (.not. precision_x8(c1(2)%cx, (3.5184372e13, 0.e0))) error stop 4_4
end
