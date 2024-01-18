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
!*  DATE                       : 09/10/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : additional TC without AC-IMPDO construct; see
!                               defect 341256)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: id

        contains

        procedure :: replicate => produceBase

        final :: finalizeBase
    end type

    type (base) :: b1_m(3), b2_m(3)

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        b%id = 0
    end subroutine

    type (base) function produceBase (b)
        class (base), intent(in) :: b

        produceBase%id = b%id
    end function
end module

program ffinal514a2
use m
    b2_m%id = (/1, 2, 3/)

    b1_m = [b2_m(1)%replicate(), b2_m(2)%replicate(), b2_m(3)%replicate()]

    print *, 'end'

    if (any (b1_m%id /= (/1, 2, 3/))) error stop 1_4
end
