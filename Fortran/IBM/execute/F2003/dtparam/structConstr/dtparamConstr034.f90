! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/15/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Data source must have target attribute.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type container (k)
        integer, kind :: k

        type(base(k,:)), pointer :: data
    end type
end module

program dtparamConstr034
use m
    type(container(8)) :: co1
    type(base(8, 20)) b1
    target b1

    logical(4), external :: precision_r8

    co1 = container(8)(b1)

    if (co1%data%n /= 20) error stop 1_4

    co1%data = base(8,20)(sqrt(1.0d0*(/(i, i=20,1,-1)/)))

    !! verify b1

    do i = 1, 20
        if (.not. precision_r8 (b1%data(i), sqrt(1.0d0*(21-i)))) error stop 1_4
    end do
end
