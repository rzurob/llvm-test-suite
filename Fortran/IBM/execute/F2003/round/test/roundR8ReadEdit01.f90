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
!*  PRIMARY FUNCTIONS TESTED   : ROUND with real*8 in READ statement
!*                             
!*
!*  DESCRIPTION                : 
!*           round with differnt edit descriptor.
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!*           9.4.1 The modes of a connection to an external file may be
!*           changed by a subsequent OPEN statement that modifies the 
!*           connection.
!* ===================================================================

  program roundR8ReadEdit01 

    implicit none

    character(18) :: r_mode 
    integer ios
    real*8 rd1, rd2

    integer, parameter::unit_r = 2 

    rd1 = 0.0D0
    rd2 = 0.0D0
    ios = 0

    ! round in up mode

    open(unit_r, file='roundR8ReadEdit01.dat', action='read', round="up")

    read(unit_r, '(f14.13, f15.13)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 100_4

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'UP') error stop 1_4 

    if(transfer(rd1, 0_8) .ne. 4608308547941528973_8) error stop 2_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 3_4

    close(unit_r)

    ! round in down mode

    open(unit_r, file='roundR8ReadEdit01.dat', action='read', round="down")

    read(unit_r, '(e14.13, e15.13)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 101_4

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'DOWN  ') error stop 4_4

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 5_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246835_8) error stop 6_4

    close(unit_r)

    ! round in zero mode

    open(unit_r, file='roundR8ReadEdit01.dat', action='read', round="zero")

    read(unit_r, '(g14.13, g15.13)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 102_4

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'ZERO  ') error stop 7_4

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 8_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 9_4

    close(unit_r)

    ! round in nearest mode

    open(unit_r, file='roundR8ReadEdit01.dat', action='read',         &
    round="nearest")

    read(unit_r, '(en14.13, en15.13)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 103_4

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'NEAREST  ') error stop 10_4

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 11_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 12_4

    close(unit_r)

    ! round in processor_defined mode

    open(unit_r, file='roundR8ReadEdit01.dat', action='read', round=   &
    "processor_defined")

    read(unit_r, '(es14.13, es15.13)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 104_4

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED  ') error stop 13_4

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 14_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 15_4

    close(unit_r)

    ! round in compatible mode

    open(unit_r, file='roundR8ReadEdit01.dat', action='read', round=    &
    "compatible")

    read(unit_r, '(f14.13, f15.13)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 105_4

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') error stop 16_4

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 17_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 18_4

    close(unit_r)

  end program roundR8ReadEdit01 
