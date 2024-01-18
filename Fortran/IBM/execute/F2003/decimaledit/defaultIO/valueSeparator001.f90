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
!*  DATE                       : 06/08/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the value separator is semicolon when
!                               decimal edit mode is COMMA in a number of
!                               situations during list-directed READ.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program valueSeparator001
    use ISO_FORTRAN_ENV

    real d1, d2, d3, d4
    logical(4), external :: precision_r4

    namelist /nml1/ d1, d2, d3

    d1 = 1.0
    d2 = 2.0
    d3 = 3.0

    write(OUTPUT_UNIT, nml1, decimal='COMMA')


    read (INPUT_UNIT, '(f4.1, TL1)', advance='no') d4

    read (INPUT_UNIT, *, decimal='COMMA') d1, d2, d3, d4

    if (.not. precision_r4(d4, 3.21)) error stop 1_4

    write (OUTPUT_UNIT, nml1)

    write (OUTPUT_UNIT, '(4f12.5)', decimal='COMMA') d1, d2, d3, d4

    read (INPUT_UNIT, *, decimal='COMMA') d1, d2, d3, d4

    write (OUTPUT_UNIT, '(4f12.5)') d1, d2, d3, d4

    end
