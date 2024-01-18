!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 24/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with real*4 in READ statement
!*                             
!*
!*  DESCRIPTION                : 
!*           test ROUND mode's interaction with  IEEE round mode during
!*           read.
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!*           9.4.1 The modes of a connection to an external file may be
!*           changed by a subsequent OPEN statement that modifies the
!*           connection.
!* ===================================================================

  program roundR4ReadEdit01a 

    use ieee_arithmetic
    implicit none

    character(18) :: r_mode 
    integer ios
    real rd1, rd2

    integer, parameter::unit_r = 2 

    rd1 = 0.0
    rd2 = 0.0
    ios = 0

    ! round in up mode

    open(unit_r, file='roundR4ReadEdit01.dat', action='read', round="up")

    call ieee_set_rounding_mode(ieee_down) ! this should not have any effect

    read(unit_r, '(f6.5, f7.5)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 100_4

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'UP') error stop 1_4 

    if(transfer(rd1, 0) .ne. 1067450788_4) error stop 2_4

    if(transfer(rd2, 0) .ne. -1080032861_4) error stop 3_4

    close(unit_r)

    ! round in processor_defined mode

    open(unit_r, file='roundR4ReadEdit01.dat', action='read')

    call ieee_set_rounding_mode(ieee_down) ! ROUND mode changed to "down" 

    read(unit_r, '(f6.5, f7.5)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 104_4

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED  ') error stop 4_4

    if(transfer(rd1, 0) .ne. 1067450787_4) error stop 5_4

    if(transfer(rd2, 0) .ne. -1080032860_4) error stop 6_4

    close(unit_r)

    open(unit_r, file='roundR4ReadEdit01.dat', action='read',      &
      & round="PROCESSOR_DEFINED")

    call ieee_set_rounding_mode(ieee_to_zero)  !ROUND mode changed to zero 

    read(unit_r, '(f6.5, f7.5)', iostat=ios) rd1, rd2

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') error stop 7_4

    if(transfer(rd1, 0) .ne. 1067450787_4) error stop 8_4

    if(transfer(rd2, 0) .ne. -1080032861_4) error stop 9_4

    close(unit_r)

    open(unit_r, file='roundR4ReadEdit01.dat', action='read',      &
      & round="PROCESSOR_DEFINED")

    call ieee_set_rounding_mode(ieee_up)  !ROUND mode changed to up 

    read(unit_r, '(f6.5, f7.5)', iostat=ios) rd1, rd2

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') error stop 10_4

    if(transfer(rd1, 0) .ne. 1067450788_4) error stop 11_4

    if(transfer(rd2, 0) .ne. -1080032861_4) error stop 12_4

    close(unit_r)

  end program roundR4ReadEdit01a 
