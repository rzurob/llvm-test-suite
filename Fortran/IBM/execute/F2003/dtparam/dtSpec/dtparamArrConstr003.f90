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
!*  DATE                       : 02/17/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Polymorphic entities in the array
!                               constructor; use the default type parameter
!                               values; type-spec used in the array construtor.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k = 8
        integer, len :: n = 36

        integer(k/2) :: id(n/2)
        real(k) :: data(n)
    end type
end module

program dtparamArrConstr003
use m
    type (base) :: b1(30)
    class(base), allocatable :: b2(:), b3(:)

    logical(4), external :: precision_r8

    allocate (b2(10), b3(10))

    do i = 1, 10
        b2(i)%id = (/(i+j*100, j=1, b2%n/2)/)
        b3(i)%id = (/(i+j*2**25, j=1, b3%n/2)/)

        b2(i)%data = (/(i*1.0d2+j, j=1, b2%n)/)
        b3(i)%data = log ((/(i*1.0d2+j, j=1, b3%n)/))
    end do

    b1 = (/base :: b2, b2(:5), b3(6:), b3/)

    !! verify the results
    do i = 1, 10
        do j = 1, 36
            if (b1(i)%id((j+1)/2) /= i+((j+1)/2)*100) error stop 1_4

            if (.not. precision_r8(b1(i)%data(j), i*1.0d2+j)) error stop 2_4
        end do
    end do

    do i = 11, 15
        do j = 1, b1%n
            if ((b1(i)%id((j+1)/2) - i + 10) /= ((j+1)/2)*100) error stop 3_4

            if (.not. precision_r8(b1(i)%data(j), (i-10)*1.0d2+j)) error stop 4_4
        end do
    end do

    do i = 16, 20
        do j = 1, b3%n
            if ((b1(i)%id((j+1)/2) - i + 10)/2**25 /= (j+1)/2) error stop 5_4

            if (.not. precision_r8(b1(i)%data(j), log((i-10)*1.0d2+j))) &
                error stop 6_4
        end do
    end do

    do i = 21, 30
        do j = 1, 36
            if ((b1(i)%id((j+1)/2) - i + 20)/2**25 /= (j+1)/2) error stop 7_4

            if (.not. precision_r8(b1(i)%data(j), log((i-20)*1.0d2+j))) &
                error stop 8_4
        end do
    end do
end
