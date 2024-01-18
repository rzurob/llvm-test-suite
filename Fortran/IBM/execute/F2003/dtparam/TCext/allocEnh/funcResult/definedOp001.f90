! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/definedOp001.f
! opt variations: -qnol -qnodeferredlp

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 09/06/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that expression is a result of defined
!                               operation.
!*
!*
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

    type(base(20,4)) function addB1B2 (b1, b2)
        class(base(*,4)), intent(in) :: b1(:), b2(:)

        dimension addB1B2(max(size(b1), size(b2)))

        !! add to the lower part of the array first
        do i = 1, min(size(b1), size(b2))
            addB1B2(i)%data = b1(i)%data + b2(i)%data
        end do

        !! assign the upper portion of the array
        if (size(b1) > size(b2)) then
            do i = size(b2)+1, size(b1)
                addB1B2(i)%data = b1(i)%data
            end do
        else
            do i = size(b1)+1, size(b2)
                addB1B2(i)%data = b2(i)%data
            end do
        end if
    end function
end module

program definedOp001
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
