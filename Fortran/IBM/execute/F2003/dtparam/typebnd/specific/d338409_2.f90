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
!*  DATE                       : 07/04/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 338409; addition case for
!                               intrinsic assignment)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real data(n)
    end type
end module

use m
    type(base(:)), allocatable :: b1
    logical(4), external :: precision_r4

    allocate (base(100) :: b1)

    b1%data(:40) = - log([(i*1.0, i = 1, 40)])
    b1%data(61:) = - b1%data(:40)

    do i = 1, 40
        if (.not. precision_r4 (b1%data(60+i), log (i*1.0))) error stop 1_4
    end do
end
