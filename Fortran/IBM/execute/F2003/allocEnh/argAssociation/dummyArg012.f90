!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/26/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Allocatable dummy-arg in an intrinsic assignment
!                               from a pointer array as expr; the type of the
!                               variable consists a type-bound defined
!                               assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: data

        contains

        procedure, private :: assgnBaseScalar
        procedure, private :: assgnBaseArrayToScalar

        generic :: assignment(=) => assgnBaseScalar, assgnBaseArrayToScalar
    end type

    contains

    subroutine assgnBaseScalar (b1, b2)
        class(base), intent(inout) :: b1
        type(base), intent(in) :: b2

        if (allocated(b2%data)) b1%data = b2%data + 1
    end subroutine

    subroutine assgnBaseArrayToScalar (b1, b2)
        class(base), intent(inout) :: b1
        type(base), intent(in) :: b2(:)

        integer, allocatable :: total

        do i = 1, size(b2)
            if (allocated(b2(i)%data)) then
                if (allocated(total)) then
                    total = total + b2(i)%data
                else
                    total = b2(i)%data
                end if
            end if
        end do

        if (allocated(total)) b1%data = total
    end subroutine


    !! this subroutine calls defined assignment
    subroutine assgnScalar (b1, b2)
        type(base), allocatable :: b1
        type(base), pointer :: b2

        if (allocated(b1) .and. associated(b2)) b1 = b2
    end subroutine


    !! this subroutine calls defined assignment
    subroutine assgnArray1 (b1, b2)
        type(base), allocatable :: b1
        type(base), pointer :: b2(:)

        if (allocated(b1) .and. associated(b2)) b1 = b2
    end subroutine


    !! this subroutine calls intrinsic assignment
    subroutine assgnArray2 (b1, b2)
        type(base), allocatable :: b1(:)
        type(base), pointer :: b2(:)

        if (associated(b2)) b1 = b2
    end subroutine


    !! this subroutine calls intrinsic assignment & defined assignment
    subroutine assgnArray3 (b1, b2)
        type(base), allocatable :: b1(:)
        type(base), pointer :: b2(:)

        if (associated(b2)) b1 = b2

        do i = lbound(b1,1), ubound(b1,1)
            b1(i) = b2(i)
        end do
    end subroutine
end module


program dummyArg012
use m
    type(base), allocatable :: b1, b2, b3(:), b4(:)
    type(base), pointer :: bptr1, bptr2(:)

    allocate (b1, b2, bptr1, bptr2(0:999))

    b1 = base(null())

    b2 = base(100)

    bptr1 = base(1000)

    do i = 1, 1000, 2
        bptr2(i)%data = i*10
    end do

    call assgnScalar (b1, bptr1)

    call assgnArray1 (b2, bptr2)

    call assgnArray2 (b3, bptr2)

    call assgnArray3 (b4, bptr2)

    !! verify b1, b2, b3 and b4
    if ((.not. allocated(b1)) .or. (.not. allocated(b2)) .or. &
        (.not. allocated(b3)) .or. (.not. allocated(b4))) error stop 20_4

    if (b1%data /= 1002) error stop 1_4

    if (b2%data /= 2500000) error stop 2_4

    if ((lbound(b3,1) /= 0) .or. (ubound(b3,1) /= 999) .or. &
        (lbound(b4,1) /= 0) .or. (ubound(b4,1) /= 999)) error stop 3_4

    do i = 0, 999, 2
        if (allocated(b3(i)%data) .or. allocated(b4(i)%data)) error stop 4_4

        if (b3(i+1)%data /= i*10+10) error stop 5_4
        if (b4(i+1)%data /= i*10+11) error stop 6_4
    end do
end
