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
!*  DATE                       : 10/24/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 327104)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: data
    end type
end module

use m
    type(base), allocatable :: b1(:)

    b1 = [base :: ]

    do i = 1, 3
        b1 = [ b1 , base(i) ]
    end do

    if (.not. allocated(b1)) error stop 1_4

    if (size(b1) /= 3) error stop 2_4

    do i = 1, 3
        if (b1(i)%data /= i) error stop 3_4
    end do
end
