! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/funcRet001.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/27/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that when a function result is used as an
!                               expr in the intrinsic assignment, the bounds of
!                               the allocatable variable is correctly set; use
!                               function that returns derived type array.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: data
    end type

    contains

    function genBaseArray (r1, lb)
        real, intent(in) ::r1(:)
        integer, intent(in) :: lb

        type(base(20,4)) genBaseArray (lb:lb+size(r1)-1)

        do i = lb, lb+size(r1)-1
            genBaseArray(i)%data = r1(i-lb+1)
        end do
    end function
end module

program funcRet001
use m
    type(base(:,4)), allocatable :: b1(:), b2(:), b3(:)

    real r1(10:100)

    logical(4), external :: precision_r4

    allocate (base(20,4) :: b1(10:100), b3(10:100))

    r1 = [(log(i*1.0), i=10, 100)]

    b1 = genBaseArray (r1, 20)

    b2 = genBaseArray (r1, 20)

    b3 = genBaseArray ([r1,r1], 30)

    if ((lbound(b1,1) /= 10) .or. (ubound(b1,1) /= 100)) error stop 1_4

    if ((lbound(b2,1) /= 1) .or. (ubound(b2,1) /= 91)) error stop 2_4

    if ((lbound(b3,1) /= 1) .or. (ubound(b3,1) /= 182)) error stop 3_4

    do i = 10, 100
        if (.not. precision_r4(b1(i)%data, r1(i))) error stop 4_4

        if (.not. precision_r4(b2(i-9)%data, r1(i))) error stop 5_4

        if (.not. precision_r4(b3(i-9)%data, r1(i))) error stop 6_4
        if (.not. precision_r4(b3(i+82)%data, r1(i))) error stop 7_4
    end do
end