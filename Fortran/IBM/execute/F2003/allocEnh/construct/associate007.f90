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
!*  DATE                       : 09/12/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test intrinsic assignment used in defined
!                               assignment in OMP directive.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        real, allocatable :: data

        contains

        procedure :: addInt
        procedure :: assignInt

        generic :: assignment(=) => assignInt
        generic :: operator(+) => addInt
    end type

    contains

    elemental subroutine assignInt (a1, i)
        class(A), intent(out) :: a1
        integer, intent(in) :: i

        a1%data = i
    end subroutine

    elemental type(A) function addInt (a1, i)
        class(A), intent(in) :: a1
        integer, intent(in) :: i

        if (allocated(a1%data)) then
            addInt%data = a1%data + i
        else
            addInt%data = i
        end if
    end function
end module

use m
    type(A), allocatable :: a1(:)
    logical(4), external :: precision_r4

    allocate (a1(0:999))

    a1 = 0

    associate (x => a1)
        !$OMP parallel do
        do i = 1, 1000
            !$OMP critical
            x = x + i
            !$OMP end critical
        end do
        !$OMP end parallel do
    end associate

    do i = 0, 999
        if (.not. precision_r4(a1(i)%data, 5.005e5)) error stop 1_4
    end do

    associate (x => a1)
        !$OMP parallel do
        do i = lbound(x,1), ubound(x,1)
            x(i) = x(i) + i
        end do
        !$OMP end parallel do
    end associate

    do i = 0, 999
        if (.not. precision_r4(a1(i)%data, 5.005e5+i)) error stop 2_4
    end do
end
