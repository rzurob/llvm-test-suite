! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/definedOp002.f
! opt variations: -qnol -qnodeferredlp

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
    type base(n1,k1)    ! (20,4)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: data
    end type

    interface operator (+)
        procedure addB1B2
    end interface

    contains

    type(base(:,4)) function addB1B2 (b1, b2)
        type(base(*,4)), intent(in) :: b1(:), b2(:)

        allocatable addB1B2(:)

        if (b1%n1 /= b2%n1) error stop 100

        if (size(b1) > size(b2)) then
            addB1B2 = (/(base(b1%n1,4)(b1(i)%data+b2(i)%data), i=1,size(b2)),&
                b1(size(b2)+1:size(b1))/)
        else
            addB1B2 = (/(base(b1%n1,4)(b1(i)%data+b2(i)%data), i=1,size(b1)),&
                b2(size(b1)+1:size(b2))/)
        end if
    end function
end module

program definedOp002
use m
    type(base(:,4)), allocatable :: b1(:)
    class(base(:,4)), allocatable :: b2(:)

    logical(4), external :: precision_r4

    b1 = (/(base(20,4)(i*1.1), i=1,50)/)

    b1 = b1 + b1 + b1

    if (size(b1) /= 50) error stop 1_4

    do i = 1, 50
        if (.not. precision_r4(b1(i)%data, i*1.1_4*3)) error stop 2_4
    end do

    !! test 2
    allocate (b2(0:99), source=(/(base(20,4)(i*i), i=1,100)/))

    b1 = b1 + b2 + b1

    if (size(b1) /= 100) error stop 3_4

    do i = 1, 50
        if (.not. precision_r4(b1(i)%data, (i*6*1.1_4+i**2))) error stop 4_4
    end do

    do i = 51, 100
        if (.not. precision_r4(b1(i)%data, i**2*1.0_4)) error stop 5_4
    end do
end