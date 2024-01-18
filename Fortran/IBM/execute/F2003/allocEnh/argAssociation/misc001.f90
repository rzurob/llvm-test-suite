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
!*  DATE                       : 10/19/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               miscellaneous test on sequence of evaluation of
!                               expr and expression within variable.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    integer function newLB (i1)
        integer, allocatable :: i1(:)

        allocatable newLB

        i1 = [(i, i=1,5)]

        newLB = ubound(i1,1) -2
    end function
end module

use m
    integer, allocatable :: i1(:)

    i1(newLB(i1): newLB(i1)+2) = [1,2,3]

    if (.not. allocated(i1)) error stop 1_4

    if (any(i1 /= [1,2,1,2,3])) error stop 2_4

    i1 = i1(newLB(i1): newLB(i1)+2)

    if (.not. allocated(i1)) error stop 3_4

    if (any(i1 /= [3,4,5])) error stop 4_4
end
