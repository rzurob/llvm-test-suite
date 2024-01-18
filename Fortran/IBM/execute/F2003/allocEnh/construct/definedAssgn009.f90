!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/7/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               pointer component in the defined assignment
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        real, pointer :: data(:,:)
    end type

    type B
        real, pointer :: data(:)
    end type

    type C
        real, pointer :: data(:)
    end type

    interface assignment(=)
        module procedure assgnA1B2
        module procedure assgnA1R1
    end interface

    contains

    subroutine assgnA1B2 (a1, b1)
        type(A), intent(inout) :: a1
        type(B), intent(in) :: b1

        if (associated(b1%data)) a1%data(1:2, 1:size(b1%data)/2) => b1%data
    end subroutine

    subroutine assgnA1R1 (a1, r1)
        type(A), intent(inout) :: a1
        type(C), target, intent(in) :: r1

        if (associated(r1%data)) a1%data(1:3, 1:size(r1%data)/3) => r1%data
    end subroutine
end module

program definedAssgn009
use m
    type(A), allocatable :: a1
    type(B), allocatable :: b1

    real, allocatable, target :: r1(:)

    logical(4), external :: precision_r4

    b1 = B(null())

    allocate(b1%data(0:100))

    a1 = A(null())

    a1 = b1

    if (.not. associated(a1%data)) error stop 1_4

    b1%data = [(i, i=1,101)]

    if (any(shape(a1%data) /= [2, 50])) error stop 2_4

    k = 1

    do j = 1, 50
        do i = 1, 2
            if (.not. precision_r4 (a1%data(i,j), k*1.0)) error stop 3_4

            k = k + 1
        end do
    end do

    !! test 2
    r1 = [(i, i=1, 200)]

    a1 = C(r1)

    if (any(shape(a1%data) /= [3,66])) error stop 4_4

    do i = 1, 66
        a1%data(:,i) = a1%data(3:1:-1,i)
    end do

    !! verify a1 and r1
    k = 1

    do j = 1, 66
        do i = 3, 1, -1
            if (.not. precision_r4(r1((j-1)*3+i), k*1.0)) error stop 5_4

            if (.not. precision_r4(a1%data(i,j), k*1.0)) error stop 6_4

            k = k + 1
        end do
    end do

    if (.not. precision_r4(r1(199), 199.0)) error stop 7_4

    if (.not. precision_r4(r1(200), 200.0)) error stop 8_4
end
