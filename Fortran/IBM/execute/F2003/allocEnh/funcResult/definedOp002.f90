!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/06/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that expression is a result of defined
!                               operation; result is allocatable array.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, allocatable :: data
    end type

    interface operator (+)
        procedure addB1B2
    end interface

    contains

    type(base) function addB1B2 (b1, b2)
        type(base), intent(in) :: b1(:), b2(:)

        allocatable addB1B2(:)

        if (size(b1) > size(b2)) then
            addB1B2 = (/(base(b1(i)%data+b2(i)%data), i=1,size(b2)),&
                b1(size(b2)+1:size(b1))/)
        else
            addB1B2 = (/(base(b1(i)%data+b2(i)%data), i=1,size(b1)),&
                b2(size(b1)+1:size(b2))/)
        end if
    end function
end module

program definedOp002
use m
    type(base), allocatable :: b1(:)
    class(base), allocatable :: b2(:)

    logical(4), external :: precision_r4

    b1 = (/(base(i*1.1), i=1,50)/)

    b1 = b1 + b1 + b1

    if (size(b1) /= 50) error stop 1_4

    do i = 1, 50
        if (.not. precision_r4(b1(i)%data, i*1.1_4*3)) error stop 2_4
    end do

    !! test 2
    allocate (b2(0:99), source=(/(base(i*i), i=1,100)/))

    b1 = b1 + b2 + b1

    if (size(b1) /= 100) error stop 3_4

    do i = 1, 50
        if (.not. precision_r4(b1(i)%data, (i*6*1.1_4+i**2))) error stop 4_4
    end do

    do i = 51, 100
        if (.not. precision_r4(b1(i)%data, i**2*1.0_4)) error stop 5_4
    end do
end
