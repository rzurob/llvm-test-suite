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
!*  DATE                       : 02/10/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Still the default type parameters for the
!                               parameterized component; but involve allocatable
!                               attribute for the components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k, n)
        integer, kind :: k = 8
        integer, len :: n = 28

        real(k) :: data(n)
    end type

    type, extends(A) :: B (m)
        integer, len :: m = 35

        integer(k) :: ids(m)
    end type

    type base (k, n)
        integer, kind :: k=4
        integer, len :: n=100

        class(A), allocatable :: data1(:, :)
        class(A(k, n)), allocatable :: data2
    end type
end module

program dtparamDefVal003
use m
    type (base), allocatable :: b2(:)

    logical(4), external :: precision_r4, precision_r8

    allocate (b2(10))

    allocate(B :: b2(1)%data1(2, 2))
    allocate (B(4,100) :: b2(2)%data2)

    allocate(b2(1)%data2, b2(2)%data1(0:1, 0:1))

    b2(1)%data2%data = log((/(i*1.0, i=1, 100)/))

    b2(2)%data2%data = sin((/(i*1.0, i=1, 100)/))

    do i = 1, 2
        do j = 1, 2
            b2(1)%data1(i,j)%data = log((/((i*10+j)*1.0d2+k, k=1,28)/))

            b2(2)%data1(i-1, j-1)%data = sin((/((i*10+j)*1.0d2+k, k=1,28)/))
        end do
    end do

    select type (x => b2(1)%data1)
        type is (B(8,*,*))
            do i = 1, 2
                do j = 1, 2
                    x(i,j)%ids = 2_8**34 * (/(k, k=1, 35)/)
                end do
            end do

        class default
            error stop 10_4
    end select

    select type (x => b2(2)%data2)
        class is (B(4,*,*))
            x%ids = (/(i*10, i=1, 35)/)

        class default
            error stop 11_4
    end select

    !! test intrinsic assgn
    b2(5:6) = b2(1:2)

    !! verify the results for b2(5:6)
    do i = 1, 2
        do j = 1, 2
            do k = 1, 28
                if (.not. precision_r8 (b2(5)%data1(i,j)%data(k), &
                    log((i*10+j)*1.0d2+k))) error stop 1_4

                if (.not. precision_r8 (b2(6)%data1(i-1,j-1)%data(k), &
                    sin((i*10+j)*1.0d2+k))) error stop 2_4
            end do

            select type (x => b2(5)%data1(i, j))
                type is (B(8,*,*))
                    if (any(x%ids/2**29 /= 32*(/(k, k=1,35)/))) error stop 3_4

                class default
                    error stop 13_4
            end select
        end do
    end do

    do i = 1, 100
        if (.not. precision_r4 (b2(5)%data2%data(i), log(i*1.0))) error stop 4_4

        if (.not. precision_r4 (b2(6)%data2%data(i), sin(i*1.0))) error stop 5_4
    end do

    select type (x => b2(6)%data2)
        type is (B(4,*,*))
            do i = 1, 35
                if (x%ids(i) /= 10*i) error stop 6_4
            end do

        class default
            error stop 14_4
    end select
end
