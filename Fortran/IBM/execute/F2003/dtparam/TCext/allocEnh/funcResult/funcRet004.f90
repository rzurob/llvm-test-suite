! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/funcResult/funcRet004.f
! opt variations: -ql

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
!*  DATE                       : 09/28/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind         :: k1
        real(k1), allocatable :: data
    end type

    contains

    function extendsArray (b1)
        type(base(8)), allocatable :: extendsArray(:)

        type(base(8)), intent(in) :: b1(:)

        extendsArray = [b1, base(8)(maxval([(b1(i)%data, i=1,size(b1))]))]
    end function

    function sumData (b1)
        real(8), allocatable :: sumData
        type(base(8)), intent(in) :: b1(:)

        sumData = 0.0d0

        do i = 1, size(b1)
            sumData = sumData + b1(i)%data
        end do
    end function
end module

program funcRet004
use m
    type(base(8)), allocatable :: b1(:)

    logical(4), external :: precision_r8
    real(8) total

!    total = log(3.6288d6) + 100.1d0*log(1.0d1)
    total = log(3.6288d6) + log(1.258925d100)

    b1 = [(base(8)(log(i*1.0d0)), i = 1, 10)]

    !! this loop should run 100 times
    do while (sumData(b1) < total)
        b1 = extendsArray (b1)
    end do


    if (size(b1) /= 111) error stop 1_4

    do i = 2, 10
        if (.not. precision_r8 (b1(i)%data, log(i*1.0d0))) error stop 2_4
    end do

    do i = 11, 111
        if (.not. precision_r8 (b1(i)%data, log(10.d0))) error stop 3_4
    end do
end
