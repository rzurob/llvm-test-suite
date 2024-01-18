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
!*  DATE                       : 05/14/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Basic test  on PASS
!                               type-bound: type point and its type-bound for
!                               calculating length between 2 points.)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtpPass001
use m
    type(point(4,:)), allocatable :: p1, p2

    class(point(4,3)), pointer :: p3, p4

    logical(4), external :: precision_r4

    type, extends(point) :: colorPoint
        integer color
    end type

    p1 = point(4,2)([1.0, 1.0])
    p2 = point(4,2)([0.0, 0.0])

    if (.not. precision_r4(sqrt(2.0_4), p1%length4(p2))) error stop 1_4

    p1 = point(4,12)(0)
    p2 = point(4,12)(1.0)

    if (.not. precision_r4(sqrt(1.2e1_4), p2%length4(p1))) error stop 2_4

    if (p1%length4(p1) >= 1.0e-10) error stop 3_4

    allocate (colorPoint(4,3) :: p3, p4)

    p3%coor = [1.0, 2.0, 3.0]
    p4%coor = [3.0, 2.0, 1.0]

    if (.not. precision_r4(sqrt(8.0_4), p3%length4(p4))) error stop 4_4
end
