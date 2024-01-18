! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/03/2006
!*
!*  DESCRIPTION                : dtparam (section 16: scope)
!                               Case: Derived type name and the type parameters
!                               are of different class in scoping.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamScope003
    !<-- derived type and type parameters are of different class
    type X (x)
        integer, kind :: x = 4

        real(x) data
    end type

    type Y (y)
        integer, len :: y

        real data(y)
    end type

    type(x(x=8)) x1
    class(x(4)), pointer :: x2(:)

    type (y(y=10)), allocatable :: y1(:)
    type (y(20)) y2

    logical (4), external :: precision_r4, precision_r8

    allocate (x2(4), y1(10))

    x1%data = 1.31d0
    x2(3)%data = .9d0**500

    y1(2)%data = (/(i*.9d0**550, i=1, 10)/)
    y2%data = 1/3.d0

    !! verify thr data
    if (.not. precision_r8(x1%data, 1.31d0)) error stop 1_4

    if (.not. precision_r4(x2(3)%data, real(.9d0**500, 4))) error stop 2_4

    do i = 1, 10
        if (.not. precision_r4(y1(2)%data(i), real(i*.9d0**550, 4))) error stop 3_4
    end do

    do i = 1, 20
        if (.not. precision_r4(y2%data(i), real(1/3.d0, 4))) error stop 4_4
    end do

    end
