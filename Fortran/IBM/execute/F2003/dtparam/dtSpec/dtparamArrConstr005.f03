! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Derived type components are of
!                               parameterized derived type; polymorphic
!                               component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real :: data(n)
    end type

    type, extends(base) :: child (k)
        integer, kind :: k

        complex(k) :: cx
    end type

    type container
        class(base(:)), allocatable :: data(:)
    end type
end module

program dtparamArrConstr005
use m
    type (container) :: co1(10)

    class (base(:)), allocatable :: b1(:), b2(:)

    logical(4), external :: precision_r4, precision_x6

    allocate (b1(0:1), source=(/base(24)((/(j*1.0, j=1, 24)/)), &
            base(24)((/(j-1.0, j=1,24)/))/))

    allocate (b2(20:21), source=(/child(31,8)((/(j/1.2e0, j=1,31)/), &
        (2.1d0, 3.2d0)), child(31,8)((/(j*2.5e0, j=1,31)/), (1.4d1, 1.4d0))/))


    !! these two calls set the lower bounds of  co1(1:3:2)%data to 1
    co1(1) = container((/base(24) :: b1/))

    select type (b2)
        class is (child(*,8))
            co1(3) = container((/child(b2%n,8) :: b2/))

        class default
            stop 20
    end select

    co1(6:5:-1) = co1(1:3:2)

    !! verify
    if ((co1(6)%data%n /= 24) .or. (co1(5)%data%n /= 31)) error stop 1_4

    do i = 1, co1(1)%data%n
        if (.not. precision_r4(co1(6)%data(1)%data(i), i*1.0)) error stop 2_4
        if (.not. precision_r4(co1(6)%data(2)%data(i), i-1.0)) error stop 3_4
    end do

    do i = 1, co1(5)%data%n
        if (.not. precision_r4(co1(5)%data(1)%data(i), i/1.2)) error stop 4_4
        if (.not. precision_r4(co1(5)%data(2)%data(i), i*2.5)) error stop 5_4
    end do

    select type (x => co1(5)%data)
        type is (child(*,8))
            if (.not. precision_x6(x(1)%cx, (2.1d0, 3.2d0))) error stop 6_4

            if (.not. precision_x6(x(2)%cx, (1.4d1, 1.4d0))) error stop 7_4

        class default
            stop 10
    end select
end
