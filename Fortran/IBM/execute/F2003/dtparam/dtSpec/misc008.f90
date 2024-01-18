!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 316411)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base24! (n)
        integer :: n = 24

        real :: data(24)
    end type

    type base31! (n)
        integer :: n = 31

        real :: data(31)
    end type

    type, extends(base31) :: child31_8! (k)
!        integer, kind :: k

        complex(8) :: cx
    end type

    type container
        class(base24), allocatable :: data(:)
    end type

    type container31
        class(base31), allocatable :: data(:)
    end type
end module

program dtparamArrConstr005
use m
    type (container) :: co1(10)
    type (container31) :: co1_(10)

    class (base24), allocatable :: b1(:)
    class (base31), allocatable :: b2(:)

    logical(4), external :: precision_r4, precision_x6

    allocate (b1(0:1), source=(/base24(24,(/(j*1.0, j=1, 24)/)), &
            base24(24,(/(j-1.0, j=1,24)/))/))

    allocate (b2(20:21), source=(/child31_8(31,(/(j/1.2e0, j=1,31)/), &
        (2.1d0, 3.2d0)), child31_8(31,(/(j*2.5e0, j=1,31)/), (1.4d1, 1.4d0))/))


    !! these two calls set the lower bounds of  co1(1:3:2)%data to 1
    co1(1) = container((/b1/))
    co1_(3) = container31((/b2/))

!    co1(6:5:-1) = co1(1:3:2)
    co1(6) = co1(1)
    co1_(5) = co1_(3)

    !! verify
    if ((co1(6)%data(1)%n /= 24) .or. (co1_(5)%data(1)%n /= 31)) error stop 1_4

    do i = 1, co1(1)%data(1)%n
        if (.not. precision_r4(co1(6)%data(1)%data(i), i*1.0)) error stop 2_4
        if (.not. precision_r4(co1(6)%data(2)%data(i), i-1.0)) error stop 3_4
    end do

    do i = 1, co1_(5)%data(1)%n
        if (.not. precision_r4(co1_(5)%data(1)%data(i), i/1.2)) error stop 4_4
        if (.not. precision_r4(co1_(5)%data(2)%data(i), i*2.5)) error stop 5_4
    end do

    select type (x => co1_(5)%data)
        type is (child31_8)
            if (.not. precision_x6(x(1)%cx, (2.1d0, 3.2d0))) error stop 6_4

            if (.not. precision_x6(x(2)%cx, (1.4d1, 1.4d0))) error stop 7_4

        class default
            stop 10
    end select
end
