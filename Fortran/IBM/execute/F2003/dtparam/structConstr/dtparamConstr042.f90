!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/23/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Polymorphic pointer to be associated with
!                               a poly data target that is of parameterized
!                               derived type; scalar case.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: name
        complex(k) :: cx
    end type
end module

module n
use m
    type container (k)
        integer, kind :: k

        class(base(k,:)), pointer :: data
    end type
end module

program dtparamConstr042
use m
use n
    type (container(8)) co1(10)

    class(base(8,:)), pointer :: b1

    class(base(4,:)), pointer :: b2

    logical(4), external :: precision_r4, precision_r8, precision_x6, &
                            precision_x8

    real(8) d1(100)

    d1 = log((/(1.0d0+i, i=1,100)/))

    allocate (b1, source=child(8,25,20)(d1(:75:3), name='b1 pointer xlftest 101', &
                cx=cmplx(d1(78), d1(88), kind=8)))


    co1(::3) = container(8)(b1)

    if ((.not. associated(co1(1)%data, co1(4)%data)) .or. &
        (.not. associated(co1(7)%data, co1(1)%data)) .or. &
        (.not. associated(co1(4)%data, co1(10)%data)) .or. &
        (.not. associated(co1(10)%data, b1))) error stop 1_4


    if (co1(1)%data%n /= 25) error stop 2_4

    do i = 1, 25
        if (.not. precision_r8(co1(4)%data%data(i), log(1.0d0+3*i-2))) &
                error stop 3_4
    end do


    select type (x => co1(10)%data)
        type is (child(8,*,*))
            if ((x%l /= 20) .or. (x%n /= 25)) error stop 5_4

            if (x%name /= 'b1 pointer xlftest 1') error stop 6_4

            if (.not. precision_x6(x%cx, cmplx(log(7.9d1), log(8.9d1), 8))) &
                    error stop 7_4

        class default
            error stop 4_4
    end select

    allocate (b2, source=child(4,20,30)(data=-1.0e-1, cx=(1.0, .1), &
            name='b2 another pointer of type child'))


    associate (x => container(4)(b2))
        if (.not. associated(x%data, b2)) error stop 10_4

        if (x%data%n /= 20) error stop 11_4

        do i = 1, 20
            if (.not. precision_r4(x%data%data(i), -.1)) error stop 12_4
        end do

        select type (y => x%data)
            class is (child(4,*,*))
                if (y%name /= 'b2 another pointer of type chi') error stop 15_4

                if (.not. precision_x8(y%cx, (1.0, .1))) error stop 16_4

            class default
                error stop 13_4
        end select
    end associate
end
