! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/15/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Correct use of data target for the case of
!                               dtparamConstr034d1.
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

    interface convert
        module procedure convertB4toB8
    end interface

    contains

    type (base(8,:)) function convertB4toB8 (b4)
        type(base(4,*)), intent(in) :: b4

        pointer convertB4toB8

        allocate (base(8,b4%n) :: convertB4toB8)

        convertB4toB8%data = b4%data
    end function
end module

program dtparamConstr034d
use m
    type (container(8)) :: co1

    type(base(4,:)), pointer :: b1

    logical(4), external :: precision_r4

    allocate (base(4, 20) :: b1)

    b1 = base(4,20)(sin(1.0e0*(/(i, i=1,20)/)))

    co1 = container(8)(convert(b1))

    !! verify
    if (co1%data%n /= 20) error stop 1_4

    do i = 1, 20
        if (.not. precision_r4(real(co1%data%data(i), kind=4), sin(1.0*i))) &
                error stop 2_4
    end do
end
