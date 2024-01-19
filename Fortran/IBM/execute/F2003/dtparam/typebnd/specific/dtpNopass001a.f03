! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/27/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type-bound procedures (Test nopass
!                               type-bound that is of module procedure)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k1, n)
        integer, kind :: k1
        integer, len :: n

        complex(k1) :: cx(n)

        contains

        procedure, nopass :: generate => generateBase
    end type

    contains

    function generateBase (cx)
        complex(4), intent(in) :: cx(:)

        type(base(4,size(cx))) generateBase

        generateBase%cx = cx
    end function
end module

program dtpNopass001a
use m
    type(base(4,:)), allocatable :: b1

    type(base(8, 10)) b2

    complex(4), allocatable :: cx1(:)

    logical(4), external :: precision_x8

    cx1 = [(cmplx(i*1.0, log(i*1.0)), i=1,25)]

    b1 = b2%generate(cx1)

    if (.not. allocated(b1)) error stop 1_4

    if (b1%n /= 25) error stop 2_4

    do i = 1, 25
        if (.not. precision_x8 (b1%cx(i), cmplx(i*1.0, log(i*1.0)))) &
            error stop 3_4
    end do
end
