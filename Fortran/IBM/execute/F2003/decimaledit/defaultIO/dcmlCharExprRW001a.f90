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
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               The decimal edit mode in data transfer
!                               statetment may be NOT affected by an invalid
!                               char-expression.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dcmlCharExprRW001a
    character(:), allocatable :: badDecMode

    real r1, r2

    logical(4), external :: precision_r4

    allocate(badDecMode, source='POINT COMMA UNDEFINED')

    !! bad decimal mode
    write (*, *, decimal=badDecMode) 1.0, 2.0

    !! set file decimal mode to COMMA
    open (10, file='test', decimal=badDecMode(7:12))

    !! bad decimal mode
    write (10, '(G12.3)', decimal=badDecMode(13:)) 1.23, 3.45

    !! set the file mode to POINT
    open (10, decimal=badDecMode(1:6), iostat=istat)

    if (istat /= 0) error stop 3_4

    rewind 10

    !! without iostat, read recovers by ignoring bad mode
    read (10, *, decimal=badDecMode(2:7)) r1, r2

    if (.not. precision_r4(r1, 1.0)) error stop 4_4
    if (.not. precision_r4(r2, 2.3e1)) error stop 5_4

    read (10, '(f12.3)', decimal=badDecMode(7:11)) r1

    if (.not. precision_r4 (r1, 3.45)) error stop 6_4
end
