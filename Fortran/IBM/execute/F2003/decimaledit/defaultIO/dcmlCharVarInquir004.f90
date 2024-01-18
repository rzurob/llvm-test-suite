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
!*  DATE                       : 06/14/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test of the scalar character variable for
!                               DECIMAL= in INQUIRE statement for preconnected
!                               files.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dcmlCharVarInquir004
use ISO_FORTRAN_ENV
    character(20) syms(3)

    character(:), allocatable :: sym4

    syms = 'xlftest 101'

    allocate(character*20 :: sym4)

    sym4(:) = ''

    inquire(10, decimal=syms(1)(2:10))

    open (10, access='direct', recl=100, form='formatted')
    open (11, access='stream')

    inquire(file='fort.10', decimal=syms(2)(:))
    inquire(file='fort.11', decimal=syms(3)(10:1))

    inquire (OUTPUT_UNIT, decimal=sym4(3:))

    if (syms(1) /= 'xUNDEFINED1') error stop 1_4

    if (syms(2) /= 'POINT') error stop 2_4

    if (syms(3) /= 'xlftest 101') error stop 3_4

    if (sym4 /= '  POINT') error stop 4_4
    end
