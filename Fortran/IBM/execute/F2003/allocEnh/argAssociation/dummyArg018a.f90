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
!*  DATE                       : 11/6/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that an allocatable dummy-arg is assigned
!                               to a dummy-procedure call.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    abstract interface
        function fr(r)
            real, allocatable :: r(:), fr(:)
        end function
    end interface

    contains

    subroutine test (r, f)
        real, allocatable :: r(:)
        procedure(fr) f

        r = f(r)
    end subroutine
end module

program dummyArg018a
use m
    real, allocatable :: r1(:)
    logical(4), external :: precision_r4

    procedure(fr) uSqrt

    call test (r1, uSqrt)

    if (.not. allocated(r1)) error stop 1_4

    if (size(r1) /= 0) error stop 2_4

    r1 = [real:: (i, 1.5, i=-99,100,2)]

    call test (r1, uSqrt)

    if (.not. allocated(r1)) error stop 3_4

    if (size(r1) /= 200) error stop 4_4


    do i = 1, 200, 2
        if (i <= 100) then
            if (.not. precision_r4 (r1(i), -1.0_4)) error stop 5_4
        else
            if (.not. precision_r4 (r1(i), sqrt(1.0*i-100.))) error stop 6_4
        end if

        if (.not. precision_r4 (r1(i+1), 1.224745_4)) error stop 7_4
    end do
end

function uSqrt (r)
use ieee_arithmetic

    real, allocatable :: r(:), uSqrt(:)

    if (allocated(r)) then
        uSqrt = sqrt(r)
    else
        uSqrt = [real :: ]
    end if

    where (ieee_is_nan(uSqrt)) uSqrt = -1.0
end function
