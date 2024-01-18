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
!*  DATE                       : 11/2/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Unlimited poly-allocatable dummy-arg used as
!                               expr in the intrinsic assignment (in select type
!                               construct).
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    subroutine assgnXtoDouble (x, d1)
        class(*), allocatable :: x(:,:)
        double precision, allocatable :: d1(:,:)

        select type (x)
            type is (integer(4))
                d1 = x

            type is (integer(8))
                d1 = x

            type is (real(4))
                d1 = x

            type is (real(8))
                d1 = x

            type is (complex(4))
                d1 = x

            type is (complex(8))
                d1 = x

            class default

        end select
    end subroutine
end module

program dummyArg015a
use m
    real(8), allocatable :: d1(:,:)

    class(*), allocatable :: x1(:,:)

    integer(8) i8

    logical(4), external :: precision_r8, precision_r4

    !! case for integer(4)
    allocate (x1(0:9, -1:8), source=reshape([(i, i=1,200)], [10,10]))

    call assgnXtoDouble (x1, d1)

    if (.not. allocated(d1)) error stop 1_4

    if (any(lbound(d1) /= [0,-1]) .or. any(ubound(d1) /= [9,8])) error stop 2_4

    k = 1

    do j = -1, 8
        do i = 0, 9
            if (.not. precision_r8 (d1(i,j), k*1.0d0)) error stop 3_4

            k = k + 1
        end do
    end do

    deallocate (x1)

    !! case for integer(8)
    allocate (x1(10,10), source=reshape([(i*2_8**33, i=1,120)], [10,10]))

    call assgnXtoDouble (x1, d1)

    if (any(lbound(d1) /= [0,-1]) .or. any(ubound(d1) /= [9,8])) error stop 4_4

    i8 = 8589934592_8
    k = 1

    do j = -1, 8
        do i = 0, 9
            if (.not. precision_r8 (d1(i,j), i8*k*1.0d0)) error stop 5_4

            k = k + 1
        end do
    end do

    deallocate (x1)

    !! case for complex(8)
    allocate (x1(5, 20), source=reshape(cmplx(log([(i*1.0d2, i=1,150)]), &
        sqrt([(i*1.0d3, i=1,150)]), 8), [5,20]))


    call assgnXtoDouble (x1, d1)

    if (any(lbound(d1) /= 1) .or. any(ubound(d1) /= [5,20])) error stop 6_4

    k = 1

    do j = 1, 20
        do i = 1, 5
            if (.not. precision_r8 (d1(i,j), dlog(k*1.0d2))) error stop 7_4

            k = k + 1
        end do
    end do

    deallocate (x1)

    !! case for others (i.e. logical); no action to be taken
    allocate (x1(200, 100), source=100< 200)

    call assgnXtoDouble (x1, d1)

    if (any(lbound(d1) /= 1) .or. any(ubound(d1) /= [5,20])) error stop 8_4

    deallocate (x1)

    !! case for real(4)
    allocate (x1(0:49, 0:19), source=sum([(i8, i8=1,200000)])*1.0)

    call assgnXtoDouble (x1, d1)

    if (any(lbound(d1) /= 0) .or. any(ubound(d1) /=[49, 19])) error stop 9_4

    do i = 0, 49
        do j = 0, 19
            if (.not. precision_r8(d1(i,j), real(2.00001e10_4, 8))) &
                error stop 10_4
        end do
    end do

    deallocate (x1)

    !! case for real(8)
    allocate (x1(50, 20), source=reshape(tan([(i*1.5d-4, i=1, 12000)]), &
        [50,20]))

    call assgnXtoDouble (x1, d1)

    if (any(lbound(d1) /= 0) .or. any(ubound(d1) /=[49, 19])) error stop 11_4

    k = 1

    do j = 0, 19
        do i = 0, 49
            if (.not. precision_r8(d1(i,j), tan(k*1.5d-4))) error stop 12_4

            k = k + 1
        end do
    end do

    deallocate (x1)

    !! case for complex(4)
    allocate (x1(20,10), source=cmplx(sinh(reshape([(i*5.e-2, i=1,250)],&
        [20,10])), cosh(1.5)))

    call assgnXtoDouble (x1, d1)

    if (any(lbound(d1) /= 1) .or. any(ubound(d1) /= [20, 10])) error stop 13_4

    k = 1

    do j = 1, 10
        do i = 1, 20
            if (.not. precision_r4(real(d1(i,j), 4), sinh(k*5.e-2))) &
                error stop 14_4

            k = k + 1
        end do
    end do
end


