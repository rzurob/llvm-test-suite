! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-09-30
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : This is a program testing the implementation of
!                               5 point stencil.  The field is initialized using
!                               cosin function and the derivative field is
!                               verified using -sine function.  Precision may be
!                               lowered to verify all the field points.  In
!                               general a high precision may be achieved by a
!                               higher resolution.
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

use five_point_stencil_mod, only: derivative
use constants_mod, only : double, pi, resolution
use distribute_fiele_mod, only: initialize_field
use verify_field_mod, only: verify_field

    implicit none

    real(double), save :: field(10*resolution)[*], field_dx(10*resolution)[*]
    intrinsic dcos
    real(double) dx

    interface
        function minus_sin(x)
        import double
            real(double), intent(in) :: x
            real(double) minus_sin
        end function
    end interface

    integer me, np

    me = this_image()
    np = num_images()

    if (np < 2) then
        print *, 'Warining: at least 2 images required to run this program'
        stop 100
    end if

    ! initialize field to be dcos() from 0 to 2pi
    call initialize_field (10*resolution, field, 0.d0, 2.0d0*pi, dcos)

    ! compute the first derivative using 5-point stencil
    call derivative (field, field_dx, 10*resolution, 0.d0, 2.0d0*pi)

    !fix four special points
    if (me == 1) then
        dx = 2*pi/np/size(field)

        field_dx(1) = -dsin(dx)
!        field_dx(2) = -dsin(2.0d0*dx) !this point is computed using 3-ptr
!        stencil instead of 5-pts; precision may be an issue

!        field_dx(size(field)-1)[np] = -dsin(2.0d0*pi - dx) !this point is
!        computed using 3-ptr stencil instead of 5-pts; precision may be an
!        issue
        field_dx(size(field))[np] = -dsin(2.0d0*pi)
    end if
    sync all

    ! verify the results
    call verify_field (field_dx, 10*resolution, minus_sin,0.0d0, 2.0d0*pi, 1.0d-9)
end

function minus_sin(x)
use constants_mod, only: double
    real(double), intent(in) :: x
    real(double) minus_sin

    minus_sin = -1.0d0*dsin(x)
end function
