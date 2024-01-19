! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.3: passed-object)
!                               Case: Procedure pointer component used in the
!                               type bound.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real(8) :: data(n)

        procedure(compute), pointer, private :: proc => null()

        contains

        procedure :: compute => computeWithExt
    end type

    contains

    real(8) function compute (b)
        class(base(*)), intent(in) :: b

        compute = 0.0d0
    end function

    real(8) function computeWithExt (b, proc)
        class(base(*)), intent(inout) :: b
        procedure(compute) proc

        b%proc => proc

        computeWithExt = b%proc()
    end function
end module


program dtparamPassObj004a
use m
    procedure (compute) :: addR8

    class(base(:)), allocatable :: b1
    type (base(10)) b2

    logical(4), external :: precision_r8

    allocate (base(100) :: b1)

    b1%data = (/(i*1.0d0, i=1,100)/)
    b2%data = (/(i*1.0d0, i=10,1,-1)/)

    if (.not. precision_r8(b1%compute(addR8), 5.05d3)) error stop 1_4
    if (.not. precision_r8(b2%compute(addR8), 5.5d1)) error stop 2_4
end


real(8) function addR8(b)
use m
    class(base(*)), intent(in) :: b

    addR8 = sum (b%data)
end function
