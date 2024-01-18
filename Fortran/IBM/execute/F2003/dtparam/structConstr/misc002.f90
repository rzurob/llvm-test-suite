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
!*  DATE                       : 02/27/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 316740)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base4! (k)

        real(4), allocatable :: data(:)
    end type

    type base8! (k)

        real(8), allocatable :: data(:)
    end type

    contains

    function genBase4 (r1)
        real(4), intent(in) :: r1(:)

        type(base4) genBase4

        genBase4 = base4(r1)
    end function

    function genBase8 (d1)
        real(8), intent(in) :: d1(:)

        type (base8) genBase8

        genBase8 = base8(data=d1)
    end function
end module

program dtparamGeneric002
use m

    interface base
        module procedure genBase4
        module procedure genBase8
    end interface

    real(4) r2(50, 80)
    real(8) d1(200)

    type (base4), allocatable :: b1(:), b2
    type (base8), pointer :: b3

    logical(4), external :: precision_r4, precision_r8

    r2 = reshape((/(i*1.2, i=1, 50*80)/), (/50, 80/))

    d1 = log(1.3d0*(/(i, i=1, 200)/))

    allocate (b1(50), source=(/(base(r2(i,:)), i=1, 50)/))

    allocate(b2)

    b2 = base((/r2/))

    allocate (b3, source=base(d1))

    !! verify results
    do i = 1, 50
        do j = 1, 80
            if (.not. precision_r4(b1(i)%data(j), r2(i, j))) error stop 2_4
        end do
    end do

    do i = 1, 4000
        if (.not. precision_r4(b2%data(i), i*1.2)) error stop 3_4
    end do

    do i = 1, 200
        if (.not. precision_r8(b3%data(i), log(1.3d0*i))) error stop 4_4
    end do
end
