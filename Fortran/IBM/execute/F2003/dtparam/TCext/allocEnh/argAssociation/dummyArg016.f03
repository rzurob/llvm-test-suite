! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/argAssociation/dummyArg016.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/3/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               A test on derived type with pointer component as
!                               the allocatable dummy-arg and used in the
!                               intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(n1,k1)    ! (20,4)
        integer, kind     :: k1
        integer, len      :: n1
        real(k1), pointer :: data(:)
    end type

    contains

    subroutine assgnA1A2 (a1, a2)
        type(A(:,4)), allocatable :: a1(:)
        type(A(*,4)), intent(in) :: a2(:)

        a1 = a2

        do i = lbound(a1,1), ubound(a1,1)
            a1(i)%data = a1(i)%data(ubound(a1(i)%data,1):&
                lbound(a1(i)%data,1):-1)
        end do
    end subroutine
end module

program dummyArg016
use m
    type(A(:,4)), allocatable :: a1(:)

    type(A(20,4)) a2(0:19)

    logical(4), external :: precision_r4

    allocate (A(20,4) :: a1(10))

    do i = 1, 20
        allocate(a2(i-1)%data(i))

        a2(i-1)%data = [(j, j=1,i)]
    end do

    call assgnA1A2 (a1, a2)

    if ((lbound(a1,1) /= 1) .or. (ubound(a1,1) /= 20)) error stop 1_4

    do i = 1, 20
        if (.not. associated(a1(i)%data, a2(i-1)%data)) error stop 2_4

        do j = 1, i
            if (.not. precision_r4(a1(i)%data(j), (i-j+1)*1.0)) error stop 3_4

            if (.not. precision_r4(a2(i-1)%data(j), (i-j+1)*1.0)) error stop 4_4
        end do
    end do

    !! 2nd test
    call assgnA1A2 (a1, a2(19:0:-1))

    if ((lbound(a1,1) /= 1) .or. (ubound(a1,1) /= 20)) error stop 5_4

    do i = 1, 20
        if (.not. associated(a1(i)%data, a2(20-i)%data)) error stop 6_4

        do j = 1, 21-i
            if (.not. precision_r4(a1(i)%data(j), j*1.0)) error stop 7_4

            if (.not. precision_r4(a2(20-i)%data(j), j*1.0)) error stop 8_4
        end do
    end do

    !!! 3rd test
    call assgnA1A2 (a1, a2(::2))

    if ((lbound(a1,1) /= 1) .or. (ubound(a1,1) /= 10)) error stop 9_4

    do i = 1, 10
        if (.not. associated(a1(i)%data, a2(2*i-2)%data)) error stop 10_4

        do j = 1, 2*i-1
            if (.not. precision_r4(a1(i)%data(j), (2*i-j)*1.0)) error stop 12_4

            if (.not. precision_r4(a2(2*i-2)%data(j), (2*i-j)*1.0)) &
                error stop 13_4
        end do

        do j = 1, 2*i
            if (.not. precision_r4(a2(2*i-1)%data(j), j*1.0)) error stop 11_4
        end do
    end do
end