!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/09/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type-parameter in
!                               declaration-type-spec: entities declaration
!                               statement: auto-array.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        real(k) :: data = 0.0
    end type

    type ut
        contains

        generic :: sum => sum4, sum8

        procedure, nopass :: sum4 => addR4
        procedure, nopass :: sum8 => addR8
    end type

    contains

    real(4) function addR4 (r, n)
        real(4), intent(in) :: r(n)
        integer, intent(in) :: n

        type(base(k=4)) b1(n)

        addR4 = 0

        b1%data = r

        do i = 1, n
            addR4 = addR4 + b1(i)%data
        end do
    end function

    real(8) function addR8 (r, n)
        real(8), intent(in) :: r(n)
        integer, intent(in) :: n

        type(base(8)) b1(n)

        addR8 = 0

        b1%data = r

        do i = 1, n
            addR8 = addR8 + b1(i)%data
        end do
    end function
end module

program kindparamDTSpec002
use m

    real(4) r1(10000), rsum
    real(8) d1(20000), dsum

    type (ut) b1
    type (ut) b2(2)

    logical(4) precision_r4, precision_r8

    call random_number (r1)
    call random_number (d1)

    rsum = b1%sum (r1, 5000)
    dsum = b2%sum (d1(::2), 4000)

    if (.not. precision_r4(rsum, sum (r1(1:5000)))) error stop 1_4
    if (.not. precision_r8(dsum, sum (d1(1:8000:2)))) error stop 2_4
end
