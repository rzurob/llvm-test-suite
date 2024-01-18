! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/16/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Derived type parameter and private access
!                               for component: elemental access for array
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k), private, dimension(n) :: data = 0.0
    end type

    interface update
        module procedure updateBase4
        module procedure updateBase8
    end interface

    interface value
        module procedure getBaseVal4
        module procedure getBaseVal8
    end interface

    contains

    subroutine updateBase4 (b, n, d)
        type (base(4, *)), intent(inout) :: b
        integer, intent(in) :: n
        real(4), intent(in) :: d

        if ((n < 1) .or. (n > b%n)) return

        b%data(n) = d
    end subroutine

    subroutine updateBase8 (b, n, d)
        type (base(8, *)), intent(inout) :: b
        integer, intent(in) :: n
        real(8), intent(in) :: d

        if ((n < 1) .or. (n > b%n)) return

        b%data(n) = d
    end subroutine

    real(4) function getBaseVal4 (b, n)
        type(base(4,*)), intent(in) :: b
        integer, intent(in) :: n

        if ((n < 1) .or. (n > b%n)) error stop 4

        getBaseVal4 = b%data(n)
    end function

    real(8) function getBaseVal8 (b, n)
        type(base(8,*)), intent(in) :: b
        integer, intent(in) :: n

        if ((n < 1) .or. (n > b%n)) error stop 8

        getBaseVal8 = b%data(n)
    end function
end module

program dtparamCompAccess003
use m
    real(4) :: r1(100)
    real(8), allocatable :: d1(:)

    class (base(4,:)), pointer :: b1(:)
    type (base(8, 165)), allocatable :: b2

    logical(4), external :: precision_r8, precision_r4

    r1 = (/(i, i=1, 100)/)
    allocate (d1(200), source=(/(dcos(i*1.d0), i=1, 200)/))

    allocate (b2)
    allocate (base(4, 20):: b1(3))

    do i = 1, 165
        if (.not. precision_r8 (value(b2, i), 0.0d0)) error stop 10_4
    end do

    do i = 1, 3
        do j = 10*i - 9, 10*i + 20
            call update (b1(i), j-10*i+10, r1(j))
        end do
    end do

    do i = 1, 200
        call update (b2, i, d1(i))
    end do

    !! verify
    do i = 1, 165
        if (.not. precision_r8(value(b2, i), d1(i))) error stop 1_4
    end do

    do i = 1, 3
        do j = 1, 20
            if (.not. precision_r4(value(b1(i), j), r1(10*i-10+j))) &
                    error stop 2_4
        end do
    end do
end
