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
!*  DATE                       : 08/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous
!                               derived type with type parameters appears in the
!                               declaration of external function return results
!                               with the type parameter of an initialization
!                               expression.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k)
        integer, kind :: k = 4

        real(k), allocatable :: data(:)
    end type
end module

use m
    type(A(8)) a1, a2

    external a1

    logical(4), external :: precision_r8

    a2 = a1 ((/1.0, 2.0/))

    if (.not. allocated(a2%data)) error stop 1_4

    if (size(a2%data) /= 2) error stop 2_4

    if (.not. precision_r8 (a2%data(1), real(1.0, 8))) error stop 3_4

    if (.not. precision_r8 (a2%data(2), real(2.0, 8))) error stop 4_4
end

function a1 (r1)
use m, only: A
    type (A(8)) a1
    real, intent(in) :: r1(2)

    allocate (a1%data(size(r1)), source=real(r1, 8))
end function
