! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/funcRet004a1.f
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
    type base(n1,k1)    ! (20,8)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: data

        contains

        procedure :: getData
    end type

    contains

    real(8) function getData (b)
        class(base(*,8)), intent(in) :: b
        allocatable getData

        if (allocated(b%data)) then
            getData = b%data
        else
            getData = tiny (1.0d0)
        end if
    end function

    function extendsArray (b1)
        type(base(:,8)), allocatable :: extendsArray(:)

        type(base(*,8)), intent(in) :: b1(:)

        extendsArray = [b1, base(20,8)(maxval([(b1(i)%getData(), i=1,size(b1))]))]
    end function

    function sumData (b1)
        real(8), allocatable :: sumData
        type(base(*,8)), intent(in) :: b1(:)

        sumData = 0.0d0

        do i = 1, size(b1)
            sumData = sumData + b1(i)%getData()
        end do
    end function
end module

program funcRet004a1
use m
    type(base(:,8)), allocatable :: b1(:)

    logical(4), external :: precision_r8
    real(8) total

!    total = log(945.d0) + 100.1d0*log(3.31d95)
    total = log(9.45d2) + log(3.31d95)

    b1 = [(base(20,8)(log(i*1.0d0)), base(20,8)(null()), i = 1, 10, 2)]

    !! this loop should run 100 times
    do while (sumData(b1) < total)
        b1 = extendsArray (b1)
    end do


    if (size(b1) /= 111) error stop 1_4

    do i = 3, 10, 2
        if (allocated(b1(i-1)%data) .or. allocated(b1(i+1)%data)) &
            error stop 11_4

        if (.not. precision_r8 (b1(i)%data, log(i*1.0d0))) error stop 2_4
    end do

    do i = 11, 111
        if (.not. precision_r8 (b1(i)%data, log(9.d0))) error stop 3_4
    end do
end
