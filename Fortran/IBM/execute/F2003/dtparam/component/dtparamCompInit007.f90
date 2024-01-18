!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/27/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.4: default init.)
!                               Case: Default initializations for the
!                               allocatable/pointer parameterized components
!                               after allocation; use more than 1 generation.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n) = -1.0
    end type

    type, extends(base) :: child
        integer(k) :: id(n) = -1
    end type

    type container
        type (base(4, 10)), pointer :: b1
        class (base(8, :)), allocatable :: b2(:)
    end type
end module

program dtparamCompInit007
use m
    type (container) co1(10)
    class (container), pointer :: co2(:)
    type (container), allocatable :: co3

    logical(4), external :: precision_r4, precision_r8

    i = 15

    allocate (co2(2), co3)

    allocate (co1(1)%b1)
    allocate (child(8, 10) :: co1(2)%b2(10))

    allocate (child(8, i) :: co2(1)%b2(2))
    allocate (base(8,i+15) :: co3%b2(20))

    !! verify the default initializations
    do i = 1, 10
        if (.not. precision_r4(co1(1)%b1%data(i), -1.0e0)) error stop 1_4
    end do

    do i = 1, 10
        do j = 1, 10
            if (.not. precision_r8(co1(2)%b2(i)%data(j), real(-1.0e0, 8)))  &
                    error stop 2_4

            select type (x => co1(2)%b2(i))
                type is (child(8, *))
                    if (x%id(j) /= -1) error stop 3_4

                class default
                    error stop 10_4
            end select
        end do
    end do

    do i = 1, 2
        do j = 1, 15
            if (.not. precision_r8(co2(1)%b2(i)%data(j), real(-1.0e0, 8))) &
                    error stop 4_4

            select type (x => co2(1)%b2(i))
                type is (child(8,*))
                    if (x%id(j) /= -1) error stop 5_4

                class default
                    error stop 20_4
            end select
        end do
    end do

    do i = 1, 20
        do j = 1, 30
            if (.not. precision_r8(co3%b2(i)%data(j), real(-1.0e0, 8))) &
                error stop 6_4
        end do
    end do
end

