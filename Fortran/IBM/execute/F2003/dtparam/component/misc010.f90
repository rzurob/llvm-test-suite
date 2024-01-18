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
!                               default initializations using the expression
!                               involving kind type parameter.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type base(k)
        integer, kind :: k
        real :: data(10) = real(1.2_4, k)
    end type

    type (base(8)) b1

    logical(4), external :: precision_r4

    do i = 1, 10
        if (.not. precision_r4(1.2_4, real(b1%data(i), 4))) error stop 1_4
    end do
    end
