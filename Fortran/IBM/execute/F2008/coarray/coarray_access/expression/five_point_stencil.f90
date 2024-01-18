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
!*  DESCRIPTION                : This test case uses a 5-pt stencil to compute
!                               the first derivative of a distributed
!                               one-dimentional field. The initial field is set
!                               up as cos(x), where x is from 0.0 to 2*pi. The
!                               resulting field should be -six(x).
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

module five_point_stencil_mod
    use constants_mod, only: double
    implicit none
    contains

    subroutine derivative (x, x_prime, n, x0, x1)
        integer, intent(in) :: n
        real(double), intent(in) :: x(n)[*]
        real(double), intent(out) :: x_prime(n)[*]
        real(double), intent(in) :: x0, x1

        integer i, me, np, left, right
        real(double) x_left1, x_left2, x_right1, x_right2
        real(double) dx

        me = this_image()
        np = num_images()

        dx = (x1 - x0)/n/np

        do i = 3, n-2
            x_left1 = x(i-1)
            x_left2 = x(i-2)
            x_right1 = x(i+1)
            x_right2 = x(i+2)

            x_prime(i) = (-x_right2 + 8.0d0*x_right1 - 8.0d0*x_left1 + x_left2) &
                         / 12.0d0/dx
        end do

        if (me == 1) then
            x_prime(1) = (x(2) - x(1))/dx !this is a rough estimate
            x_prime(2) = (x(3) - x(1))/2.0d0/dx

            x_prime(n-1) = (-x(1)[me+1] + 8.0d0*x(n) - 8.0d0*x(n-2) + x(n-3)) &
                            /12.0d0/dx

            x_prime(n) = (-x(2)[me+1] + 8.0d0*x(1)[me+1] - 8.0d0*x(n-1) + x(n-2)) &
                            /12.0d0/dx

        else if (me == np) then
            x_prime(1) = (-x(3) + 8.0d0*x(2) - 8.0d0*x(n)[me - 1] + &
                          x(n-1)[me-1]) / 12.0d0/dx

            x_prime(2) = (-x(4) + 8.0d0*x(3) - 8.0d0*x(1) + &
                          x(n)[me-1]) / 12.0d0/dx

            x_prime(n-1) = (x(n) - x(n-2))/2.0d0/dx
            x_prime(n) = (x(n) - x(n-1))/dx  !this is a rough estimate
        else
            x_prime(1) = (-x(3) + 8.0d0*x(2) - 8.0d0*x(n)[me - 1] + &
                          x(n-1)[me-1]) / 12.0d0/dx

            x_prime(2) = (-x(4) + 8.0d0*x(3) - 8.0d0*x(1) + &
                          x(n)[me-1]) / 12.0d0/dx

            x_prime(n-1) = (-x(1)[me+1] + 8.0d0*x(n) - 8.0d0*x(n-2) + x(n-3)) &
                            /12.0d0/dx

            x_prime(n) = (-x(2)[me+1] + 8.0d0*x(1)[me+1] - 8.0d0*x(n-1) + x(n-2)) &
                            /12.0d0/dx
        end if

        sync all
    end subroutine
end module
