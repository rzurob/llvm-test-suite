!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.3: passed-object)
!                               Case: Procedure pointer components can be
!                               assigned to each other if with the same
!                               interface characteristic.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real(8) :: data(n)

        procedure (compute), pointer :: intrinsic => null()
        procedure (compute), pointer :: other => null()
    end type

    contains

    real(8) function compute (b)
        class(base(*)), intent(in) :: b

        compute = 0.0d0
    end function
end module

program dtparamPassObj004
    call simpleTest
end


subroutine simpleTest
use m
    procedure(compute) :: add, addViaSum

    type (base(20)) b1

    logical(4), external :: precision_r8

    b1%data = (/(dcos(i*1.0d0), i=1, 20)/)
    b1%intrinsic => addViaSum
    b1%other => add

    if (.not. precision_r8(b1%other(), b1%intrinsic())) error stop 1_4

    b1%other => b1%intrinsic

    if (.not. precision_r8(b1%other(), b1%intrinsic())) error stop 2_4
end subroutine


real(8) function add (b)
use m
    class(base(*)), intent(in) :: b

    add = 0.0d0

    do i = 1, b%n
        add = add + b%data(i)
    end do
end function

real(8) function addViaSum(b)
use m
    class(base(*)), intent(in) :: b

    addViaSum = 0.0d0

    addViaSum = sum(b%data)
end function
