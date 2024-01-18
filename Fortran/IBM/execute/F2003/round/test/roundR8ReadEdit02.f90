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
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!*           test ROUND edit descriptor in READ statement.
!* ===================================================================

  program roundR8ReadEdit02 

    implicit none

    character(18) :: r_mode 
    integer ios
    real*8 rd1, rd2

    integer, parameter::unit_r = 2 

    rd1 = 0.0
    rd2 = 0.0
    ios = 0

    ! round in up mode, suing edit descriptor.

    open(unit_r, file='roundR8ReadEdit02.dat', action='read', round="up")

    read(unit_r, '(RU, f14.13, f15.13)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 100_4

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'UP') error stop 1_4 

    if(transfer(rd1, 0_8) .ne. 4608308547941528973_8) error stop 2_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 3_4

    ! round in down mode, using edit descriptor

    rewind(unit_r)

    read(unit_r, '(RD, f14.13, f15.13)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 101_4

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 4_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246835_8) error stop 5_4

    ! round in zero mode

    rewind(unit_r)

    read(unit_r, '(f14.13, f15.13)', iostat=ios, round="zero") rd1, rd2

    if(ios /= 0) error stop 102_4

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 6_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 7_4

    ! round in nearest mode

    rewind(unit_r)

    read(unit_r, '(RN, f14.13, f15.13)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 103_4

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 8_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 9_4

    ! round in processor_defined mode
 
    rewind(unit_r)

    read(unit_r, '(RP, f14.13, f15.13)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 104_4

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 10_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 11_4

    ! round in compatible mode

    rewind(unit_r)

    read(unit_r, '(RC, f14.13, f15.13)', iostat=ios) rd1, rd2

    if(ios /= 0) error stop 105_4

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 12_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 13_4

    r_mode="down" 

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'UP') error stop 14_4

    close(unit_r)

  end program roundR8ReadEdit02 
