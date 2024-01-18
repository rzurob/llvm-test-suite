! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-09-29
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : This unit defines a module to distribute a
!                               field using an external function input.
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

module constants_mod
    implicit none
    integer, parameter :: resolution = 1000
    integer, parameter :: double = selected_real_kind(p=12)
    real(double), parameter :: pi = 4.0d0*atan(1.0d0)
end module

module distribute_fiele_mod
use constants_mod, only: double
    implicit none
    abstract interface
        function dist (x)
            import
            real(double), intent(in) :: x
            real(double) dist
        end function
    end interface

    contains

    ! this routine does a field distribution based on input function f.  The
    ! field range is from x0 to x1 distributed evenly across np images.
    subroutine initialize_field (n, field, x0, x1, f)
        integer, intent(in) :: n
        real(double), intent(out) :: field(n)[*]
        real(double), intent(in) :: x0, x1
        procedure(dist) :: f
        real(double) dx, xi

        integer :: i, me, np, k

        me = this_image()
        np = num_images()

        dx = (x1 - x0)/n/np

        !compute field values concurrently on all images
        do i = 1, n
            xi = x0 + ((me-1)*n +i)*dx
            field(i) = f(xi)
        end do
        sync all
    end subroutine
end module

