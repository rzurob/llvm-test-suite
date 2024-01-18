! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-08-11
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : test case for intrinsic function SUM.  Use
!                                real(8) as the actual data type.  The test is
!                                for SUM with DIM argument.  The computations
!                                are for the areal of a function f(x) of a
!                                span between 0.0 and 1.0.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module compute_mod

    implicit none
    abstract interface
        real(8) function f (x)
            real(8), intent(in) :: x
        end function
    end interface

    contains

    ! this routine computes an area of f(x) based on the points being passed in
    ! and store the computation in a result array
    subroutine compute (x, fx, res)
        real(8), intent(in) :: x(:)
        procedure(f) fx
        real(8), intent(out) :: res(:)

        integer i
        real(8) dx, x_mean

        if (size(x) <= 1) return

        dx = x(2) - x(1)

        do i = 1, size(x) - 1
            x_mean = 0.5d0*(x(i) + x(i+1))
            res(i) = fx(x_mean) * dx
        end do
    end subroutine
end module

program sumCoarray02
use compute_mod
    implicit none
    integer, parameter :: resolution = 100
    procedure(f) :: linear, myFx
    real(8), save :: coords(resolution+1)[*]

    real(8), save :: compute_res(resolution, 2)[*]
    real(8), save :: sumVal(2)[*]

    integer :: np, me, left

    me = this_image()
    np = num_images()

    if (me == 1) then
        left = np
    else
        left = me - 1
    end if

    call setup_coords
    sync all

    ! compute area for the function linear
    call compute (coords, linear, compute_res(:,1))

    ! compute area for the function myFx
    call compute (coords(:)[left], myFx, compute_res(:,2))
    sync all

    ! verify results using SUM
    sumVal = sum (compute_res, dim=1)

    if (.not. precision_test(sumVal(1), (2.d0*me-1.0d0)/np/np, 1.0d-8)) then
        print *, sumVal(1), (2.d0*me-1.0d0)/np/np
        print *, 'validating sumVal(1) failed on image', me
        error stop 1
    end if

    if (.not. precision_test(sumVal(2), &
        2.0d0*(dcos((left-1)*1.0d0/np) - dcos(left*1.0d0/np)), 5.0d-7)) then

        print *, sumVal(2),2.0d0*(dcos((left-1)*1.0d0/np) - dcos(left*1.0d0/np))
        print *, 'validating sumVal(2) failed on image', me
        error stop 1
    end if

    sync all
    contains

    ! this routine set up coordinates concurrently
    ! the method is by dividing coordinates [0.0, 1.0] by total of np*resolution
    ! sections, and each image is holding resolution+1 data
    subroutine setup_coords
        integer :: i
        real(8) delta

        delta = 1.0d0/resolution/(np*1.0d0)

        coords(1) = (me-1)*1.0d0/np

        do i = 2, resolution+1
            coords(i) = coords(i-1) + delta
        end do
    end subroutine

    ! precision routine with controlled precision tolerance
    logical function precision_test(a,b,tol)
        real(8), intent(in) :: a,b,tol

        precision_test = abs(a - b) <= abs(a+b)*tol*0.5d0
    end function
end

real(8) function linear (x)
    real(8), intent(in) :: x

    linear = 2.0 * x
end function

real(8) function myFx (x) result(res)
    real(8), intent(in) :: x

    res = 2.0 * dsin(x)
end function
