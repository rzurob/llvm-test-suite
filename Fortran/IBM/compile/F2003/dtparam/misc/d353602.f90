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
!*  DATE                       : 08/31/2009
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 353602)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type X (n)
        integer, len :: n
        integer i(n)
    end type

    contains

    subroutine tt (x1, x2)
        type(X(*)), allocatable :: x1
        type(X(*)), pointer :: x2
    end subroutine
end module

use m
    type(X(:)), allocatable :: x1
    type(X(2)), allocatable :: xx1
    type(X(:)), pointer :: x2
    type(X(10)), pointer :: xx2

    call tt(x1, xx2)     !<-- illegal; should be diagnosed
    call tt(xx1, x2)     !<-- illegal; should be diagnosed
end
