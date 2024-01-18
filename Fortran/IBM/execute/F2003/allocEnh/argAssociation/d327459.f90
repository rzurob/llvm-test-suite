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
!*  DESCRIPTION                : miscellaneous (defect 327459)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: i
    end type
end module

use m
    type(base), allocatable, target :: b1(:)
    type(base), pointer :: b2(:)

    b1 = [(base(i), i = 1, 10)]

    b2 => b1(::2)

    b1 = b2

    do i = 1, 5
        if (b1(i)%i /= 2*i-1) error stop 1_4
    end do
end
