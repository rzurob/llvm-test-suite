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
!*  DATE                       : 10/31/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 327419)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        complex(8), allocatable :: cx(:)
    end type

    contains

    subroutine copyData (a1, cx)
        type(A), value :: a1
        complex(8), allocatable :: cx(:)

        call move_alloc (a1%cx, cx)
    end subroutine
end module

use m
    type(A), allocatable :: a1
    complex(8), allocatable :: cx(:)

    logical(4), external :: precision_x6

    a1 = A(cmplx([(i, i=1,10)], kind=8))

    call copyData (a1, cx)

    do i = 1, 10
        if (.not. precision_x6 (a1%cx(i), cmplx(i, kind=8))) error stop 1_4
        if (.not. precision_x6 (cx(i), cmplx(i, kind=8))) error stop 2_4
    end do
end
