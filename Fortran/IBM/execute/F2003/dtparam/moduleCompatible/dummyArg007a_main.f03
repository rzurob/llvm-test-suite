! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/allocEnh/argAssociation/dummyArg007a.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/18/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the derived type dummy allocatable
!                               argument used in the intrinsic assignment:
!                               scalar case of dummyArg007.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
program dummyArg007
use m1
use m, only: copyBase
    type(base(:,8,4)), allocatable :: b1, b2

    type(dataType(4,:)), allocatable :: d1

    logical, external :: less, precision_r8

    d1 = dataType(4,20)([(i, i=1,100)])

    call moveData (d1, b1)

    call setMask (b1, 20.2d0, less)

    call copyBase (b2, b1)

    call b1%reduce


    !! verify d1, b1 and b2
    if (allocated(d1%data)) error stop 2_4

    if (associated(b1%mask) .or. (.not. associated(b2%mask))) error stop 3_4

    if ((size(b1%data) /= 20) .or. (size(b2%data) /= 100) .or. &
        (size(b1%mask) /= 100)) error stop 4_4

    do j = 1, 20
        if (.not. precision_r8 (b1%data(j), j*1.0d0)) error stop 5_4

        if (.not. precision_r8 (b2%data(j), j*1.0d0)) error stop 6_4

        if (.not. b2%mask(j)) error stop 7_4
    end do

    do i = 21, 100
        if (b2%mask(i)) error stop 8_4

        if (.not. precision_r8 (b2%data(i), i**1.0d0)) error stop 9_4
    end do

    deallocate (b2%mask)
end

logical function less (d1, d2)
    double precision d1, d2
    less = d1 < d2
end function
