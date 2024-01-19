! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/07/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of different forms of expressions for
!                               data component in structure constructor: named
!                               constants, type parameter name and type
!                               parameter inquiry.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        integer(1) :: kindVal
    end type

    type container (k,n)
        integer, kind :: k, n

        type (base(k,n)) :: data = base(k,n)(data=(/(-1.0*i, i=1,n)/), kindVal=k)
    end type

    type (base(8, 20)) b_const

    parameter (b_const = base(8,20)(kindVal=8, &
        data=(/(mod(3.65d2, i*1.0d0), i=1,20)/)))
end module

program dtparamConstr026
use m
    type (base(4, :)), allocatable :: b1

    type (container(8,20)) :: co1(3)

    logical(4), external :: precision_r4, precision_r8

    allocate (b1, source=base(4,31)((/b_const%data, b_const%data(1:11)/), &
            b_const%k/2))


    co1 = (/container(8,20)(), container(8,20)(b_const), &
        container(8,20)(base(8,20)(b_const%data*2.0d0, b1%k*2))/)


    !! verify
    if (b1%n /= 31) error stop 1_4

    do i = 1, 20
        if (.not. precision_r4 (b1%data(i), real(mod(3.65d2, i*1.0d0), 4))) &
                error stop 2_4
    end do

    do i = 21, 31
        if (.not. precision_r4(b1%data(i), real(mod(3.65d2,(i-20)*1.0d0),4))) &
                error stop 3_4
    end do

    if (b1%kindVal /= b1%k) error stop 4_4


    do i = 1, 20
        if (.not. precision_r8(co1(1)%data%data(i), -1.0d0*i)) error stop 5_4

        if (.not. precision_r8(co1(2)%data%data(i), mod(3.65d2, i*1.0d0))) &
                error stop 6_4

        if (.not. precision_r8(co1(3)%data%data(i), 2.0d0*mod(3.65d2, i*1.0d0)))&
                error stop 7_4
    end do

    if (any(co1%data%kindVal /= 8)) error stop 8_4
end
