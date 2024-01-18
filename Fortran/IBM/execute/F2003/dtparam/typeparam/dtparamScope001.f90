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
!*  DATE                       : 01/02/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam
!                               Case: Derived type parameters are within the
!                               scope of derived type definition.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamScope001
    integer(8), parameter ::  k = 8

    type base (k)
        integer, kind :: k = 4

        real(k) data
    end type

    type (base(k)) b1
    type (base) b2
    type (base(selected_int_kind(10))) b3

    if (precision(b1%data) /= precision(1.0d0)) error stop 1_4

    if (precision(b2%data) /= precision(1.0e0)) error stop 2_4

    if (precision(b3%data) /= precision(b1%data)) error stop 3_4
    end
