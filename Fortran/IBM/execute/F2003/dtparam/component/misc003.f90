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
!*  DATE                       : 01/18/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A! (lb, ub)
        integer :: lb, ub

        integer :: i1(10:100) = 1
    end type

    type(A), parameter :: a_const = A(10, 100)

    type B
        real data (a_const%lb:a_const%ub)
    end type
end module

program dtparamCompBound001
use m
    type(B) b1

    if(size(b1%data) /= 91) error stop 1_4
end
