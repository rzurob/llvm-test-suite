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
!*  DATE                       : 10/25/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the allocatable variable dummy-arg assigned
!                               to pointer dummy-arg array.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
        real, pointer :: data
        complex, allocatable :: cx
    end type

    contains

    subroutine copyOver (b1, b2)
        type(base), allocatable :: b1(:,:)
        type(base), pointer :: b2(:,:)

        b1 = b2
    end subroutine
end module

program dummyArg011
use m

    logical(4), external :: precision_r4, precision_x8
    type(base), allocatable :: b1(:,:)
    type(base), pointer :: b2(:,:)

    allocate (b2(0:2, 0:5), b1(2, 5))

    do i = 0, 2
        do j = 0, 5
            b2(i,j)%id = i*10 + j
            b2(i,j)%cx = cmplx(i, j)
            allocate(b2(i,j)%data)

            b2(i,j)%data = b2(i,j)%id
        end do
    end do

    call copyOver (b1, b2)

    deallocate (b2)

    allocate (b2(2:4, 2:7))

    if (any(lbound(b1) /= 0) .or. any(ubound(b1) /= [2,5])) error stop 1_4

    do i = 0, 2
        do j = 0, 5
            if (b1(i,j)%id /= 10*i+j) error stop 2_4

            if ((.not. associated(b1(i,j)%data)) .or. &
                (.not. allocated(b1(i,j)%cx))) error stop 3_4

            if (.not. precision_r4 (b1(i,j)%data, b1(i,j)%id*1.0)) &
                error stop 4_4

            if (.not. precision_x8 (b1(i,j)%cx, cmplx(i,j,4))) error stop 5_4
        end do
    end do


    !! test 2: reset values of b2 and do the assignment again
    do i = 2, 4
        do j = 2, 7
            b2(i,j)%id = i*10 + j
            b2(i,j)%cx = log(cmplx(i,j,4))

            b2(i,j)%data => b1(i-2, j-2)%data
        end do
    end do

    call copyOver (b1, b2)

    deallocate (b2)

    do i = 0, 2
        do j = 0, 5
            if (b1(i,j)%id /= 10*i+j+22) error stop 7_4

            if ((.not. associated(b1(i,j)%data)) .or. &
                (.not. allocated(b1(i,j)%cx))) error stop 8_4

            if (.not. precision_r4 (b1(i,j)%data, i*1.0e1+j)) &
                error stop 9_4

            if (.not. precision_x8 (b1(i,j)%cx, &
                cmplx(log(sqrt(1.0e0*((i+2)**2+(j+2)**2))), &
                atan((j+2.0)/(i+2.0)),4))) error stop 10_4
        end do
    end do

end
