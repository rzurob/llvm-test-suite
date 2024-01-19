! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/14/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that intrinsic assignment would not apply
!                               if defined assignment exists.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, pointer :: i(:) => null()

        contains

        final :: finalizeBase
    end type

    interface assignment(=)
        procedure assgnB1B2
    end interface

    contains

    elemental subroutine finalizeBase (b)
        type(base), intent(inout) :: b

        if (associated(b%i)) deallocate (b%i)
    end subroutine

    subroutine assgnB1B2 (b1, b2)
        type(base), intent(out) :: b1(:)
        type(base), intent(in) :: b2(:)

        do i = 1, min(size(b1), size(b2))
            if (associated(b2(i)%i)) allocate(b1(i)%i(size(b2(i)%i)), source=b2(i)%i)
        end do
    end subroutine
end module

use m
    type(base), allocatable :: b1, b11, b2(:), b3(:)

    !! for scalars intrinsic assignments apply
    b1 = base(null())
    b11 = b1

    if ((.not. allocated(b1)) .or. (.not. allocated(b11))) error stop 1_4

    if (associated(b1%i) .or. associated(b11%i)) error stop 2_4

    allocate(b11%i(10))

    b1 = b11

    if (.not. associated(b1%i, b11%i)) error stop 3_4
    if (size(b1%i) /= 10) error stop 4_4

    allocate(b2(15), b3(20))

    do i = 1, 20, 2
        allocate(b3(i)%i(i), source=i)
    end do

    !! if rank-one array appear on both RHS and LHS, defined assignments apply
    b2 = b3

    if (size(b2) /= 15) error stop 5_4

    do i = 1, 13, 2
        if ((.not. associated(b2(i)%i)) .or. associated(b2(i)%i, b3(i)%i)) &
            error stop 6_4

        if (size(b2(i)%i) /= i) error stop 7_4

        if (any(b2(i)%i /= i)) error stop 8_4

        if (associated(b2(i+1)%i)) error stop 9_4
    end do

    if (.not. associated(b2(15)%i) .or. associated(b2(15)%i, b3(15)%i)) &
        error stop 10_4


    b2 = b3(::2)

    if (size(b2) /= 15) error stop 11_4

    do i = 1, 10
        if ((.not. associated(b2(i)%i)) .or. associated(b2(i)%i, b3(2*i-1)%i)) &
            error stop 12_4

        if (size(b2(i)%i) /= 2*i-1) error stop 13_4

        if (any(b2(i)%i /= 2*i-1)) error stop 14_4
    end do

    !! last test: if RHS is a scalar, then intrinsic assignment applies
    b2 = b11

    if (size(b2) /= 15) error stop 15_4

    do i = 1, 15
        if (.not. associated(b2(i)%i, b1%i)) error stop 16_4
    end do
end

