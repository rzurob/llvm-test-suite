!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/30/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               generic type bound (Use of nopass bindings for
!                               generic type bound).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        complex(k) :: data(n)

        contains

        generic :: makeObj => gen4, gen8, gen16
        procedure, private, nopass :: gen4 => genBase4
        procedure, private, nopass :: gen8 => genBase8
        procedure, private, nopass :: gen16 => genBase16
    end type

    contains

    function genBase4 (cx)
        complex(4), intent(in) :: cx(:)

        type(base(4,size(cx))) genBase4

        genBase4%data = cx
    end function

    function genBase8 (cx)
        complex(8), intent(in) :: cx(:)

        type(base(8,size(cx))) genBase8

        genBase8%data = cx
    end function

    function genBase16 (cx)
        complex(16), intent(in) :: cx(:)

        type(base(16,size(cx))) genBase16

        genBase16%data = cx
    end function
end module

program dtpGeneric001
use m
    type(base(4,:)), allocatable :: b1
    type(base(8,:)), pointer :: b2(:)
    class(base(16,10)), allocatable :: b3

    complex(4) cx4(100)
    complex(8), allocatable :: compDouble(:)

    logical(4), external :: precision_x8, precision_x6, precision_x3

    cx4 = [(cmplx(i*0.5, i*1.2), i=1,100)]

    allocate (base(4,1) :: b1)

    allocate (base(8, 25) :: b2(10))

    allocate (b3, source=b1%makeObj([complex(16) :: (i, i=1,10)]))

    b1 = b1%makeObj(cx4)

    do i = 1, 10
        compDouble = [(cmplx(j,j,8), j = 1+i,25+i)]

        b2(i) = b2%makeObj (compDouble)
    end do

    !! verify results
    if (b1%n /= 100) error stop 1_4

    do i = 1, 100
        if (.not. precision_x8 (cx4(i), b1%data(i))) error stop 2_4
    end do

    do i = 1, 10
        do j = 1, 25
            if (.not. precision_x6(cmplx(j+i,j+i,8), b2(i)%data(j))) error stop 3_4
        end do
    end do

    do i = 1, 10
        if (.not. precision_x3(b3%data(i), cmplx(i,kind=16))) error stop 4_4
    end do
end
