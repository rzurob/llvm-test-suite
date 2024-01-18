! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/zeroSize/zeroSizeArray006.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/29/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use of the intrinsic functions to produce the
!                               zero-sized arrays: matmul.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: data(:,:)
    end type
end module

program zeroSizeArray006
use m
    integer, allocatable :: i1(:,:)
    complex, allocatable :: cx1(:,:)

    type(base(:,4)), allocatable :: b1

    logical(4), external :: precision_r4

    i1 = reshape((/(i, i=1,6)/), (/3,2/))

    cx1 = reshape((/(1.0,2.0)/), (/2,0/))

    b1 = base(20,4)(reshape((/(i, i=1,9)/), (/3,3/)))

    if ((.not. allocated(b1)) .or. (.not. allocated(b1%data))) error stop 1_4

    k = 1
    do j = 1, 3
        do i = 1, 3
            if (.not. precision_r4(b1%data(i,j), k*1.0_4)) error stop 2_4

            k = k + 1
        end do
    end do

    if (any(pack(i1, .true.) /= (/1,2,3,4,5,6/))) error stop 3_4

    !! now assign zero-sized array to b1%data
    b1%data = matmul (i1, cx1)

    if (.not. allocated(b1%data)) error stop 4_4

    if (size(b1%data) /= 0) error stop 5_4

    if (any(shape(b1%data) /= (/3,0/))) error stop 6_4

    !! now use zero-size array in the arithmetics

    i1 = reshape((/0/), (/2,0/))

    if (any(shape(i1) /= shape(cx1))) error stop 7_4

    b1%data = i1 * cx1

    if (.not. allocated(b1%data)) error stop 8_4

    if (any(shape(b1%data) /= shape(cx1))) error stop 9_4
end
