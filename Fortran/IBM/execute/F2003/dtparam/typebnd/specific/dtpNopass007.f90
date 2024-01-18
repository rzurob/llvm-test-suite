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
!*  DATE                       : 05/03/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Nopass binding for
!                               allocatable dummy-arg.)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)

        contains

        procedure, nopass :: update => getAnewBaseObj
    end type

    contains

    subroutine getAnewBaseObj (b1, d1)
        class(base(8,:)), allocatable, intent(inout) :: b1
        real(8), intent(in) :: d1(:)

        if (allocated(b1)) then
            if (b1%n /= size(d1)) then
                deallocate (b1)
            else
                b1%data = d1
            end if
        end if

        if (.not. allocated(b1)) then
            allocate (b1, source=base(8,size(d1))(d1))
        end if
    end subroutine
end module

program dtpNopass007
use m
    class(base(8,:)), allocatable :: b1
    class(base(4,:)), allocatable :: b2

    double precision d1(2:50)

    real(8), allocatable :: d2(:)

    logical(4), external :: precision_r8

    allocate (base(4,0):: b2)

    do i = 1, 49
        d1(i+1) = log((i+1)*1.2d-2)
    end do

    call b2%update (b1, d1)

    !! verify b1
    if (.not. allocated(b1)) error stop 1_4

    if (b1%n /= 49) error stop 2_4

    do i = 1, 49
        if (.not. precision_r8 (b1%data(i), log((i+1)*1.2d-2))) error stop 3_4
    end do

    d2 = [(j, j=1,100)]

    !! update b1
    call b1%update (b1, d2)

    if (b1%n /= 100) error stop 4_4

    do i = 1, 100
        if(.not. precision_r8 (b1%data(i), i*1.0d0)) error stop 5_4
    end do

    !! update b1 again
    call b1%update (b1, sqrt(d2))

    do i = 1, 100
        if (.not. precision_r8 (b1%data(i), sqrt(i*1.0d0))) error stop 6_4
    end do
end
