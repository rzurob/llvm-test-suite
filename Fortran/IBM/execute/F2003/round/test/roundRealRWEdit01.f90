!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ/WRITE statement
!*
!*  DESCRIPTION                :
!*          test different rounding mode with external file for data
!*          input and output data transfer.
!* ===================================================================

  program roundRealRWEdit01

    implicit none

    character(18) :: r_mode
    real*8 rd1, rd2

    integer, parameter::unit_r = 2
    integer, parameter::unit_w = 3

    rd1 = 0.0D0
    rd2 = 0.0D0

    ! round in up mode

    open(unit_r, file='roundRealRWEdit01.dat', action='read')
    open(unit_w, file='roundRealRWEdit01.out', action='write')

    read(unit_r, '(RU, f14.13, f15.13)') rd1, rd2

    if(transfer(rd1, 0_8) .ne. 4608308547941528973_8) error stop 1_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 2_4

    ! edit descriptor takes precedence over specifier in one data transfer
    ! statement

    write(unit_w, '(a10,1x,RU,f15.13,1x,f16.13)',round="down") "round up", &
    rd1, rd2

    rewind(unit_r)

    ! round in down mode

    read(unit_r, '(RD, f14.13, f15.13)') rd1, rd2

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 3_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246835_8) error stop 4_4

    write(unit_w, '(a10,1x,RD, f15.13,1x,f16.13)', round="up") "round down", &
    rd1, rd2

   rewind(unit_r)

    ! round in zero mode

    read(unit_r, '(RZ, f14.13, f15.13)') rd1, rd2

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 5_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 6_4

    write(unit_w, '(a10,1x,RZ, f15.13,1x,f16.13)', round="up") "round zero", &
    rd1, rd2

   rewind(unit_r)

    ! round in nearest mode

    read(unit_r, '(RN, f14.13, f15.13)') rd1, rd2

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 11_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 12_4

    write(unit_w, '(a13,1x,RN,f15.13,1x,f16.13)', round="up") "round nearest", &
    rd1, rd2

   rewind(unit_r)

    ! round in processor_defined mode

    read(unit_r, '(RP, f14.13, f15.13)') rd1, rd2

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 14_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 15_4

    write(unit_w, '(a23,1x,RP, f15.13,1x,f16.13)', round="up")             &
    "round processor_defined", rd1, rd2

   rewind(unit_r)

    ! round in compatible mode

    read(unit_r, '(RC, f14.13, f15.13)') rd1, rd2

    if(transfer(rd1, 0_8) .ne. 4608308547941528972_8) error stop 17_4

    if(transfer(rd2, 0_8) .ne. -4615063488913246836_8) error stop 18_4

    write(unit_w, '(a23,1x,RC, f15.13,1x,f16.13)', round="up")          &
    "round compatible" , rd1, rd2

    close(unit_r)
    close(unit_w)

  end program roundRealRWEdit01
