! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/25/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test TRANSFER for derived type with allocatable,
!                               pointer components (descriptors preserved).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        real, allocatable :: data
        real, pointer :: val
    end type
end module

module m1
    type B
        real, allocatable :: val
        real, pointer :: data
    end type
end module

use m
use m1
    type(A), allocatable :: a1(:)

    type (B), allocatable :: b1(:)

    logical(4), external :: precision_r4

    b1 = [(b(null(), null()), i=1, 100)]

    do i = 1, 100, 2
        b1(i)%val = i*1.0

        allocate(b1(i+1)%data)
        b1(i+1)%data = i*1.0e2
    end do

    a1 = transfer(b1, A(null(), null()), size(b1))

    if (.not. allocated(a1)) error stop 1_4

    if (size(a1) /= 100) error stop 2_4

    do i = 1, 10, 2
        if (.not. allocated(a1(i)%data)) error stop 3_4

        if (allocated(a1(i+1)%data)) error stop 4_4

        if (.not. precision_r4(a1(i)%data, i*1.0)) error stop 5_4

        if (associated(a1(i)%val)) error stop 6_4

        if (.not. associated(a1(i+1)%val, b1(i+1)%data)) error stop 7_4

        if (.not. precision_r4(a1(i+1)%val,i*1.0e2)) error stop 8_4

    end do

end
