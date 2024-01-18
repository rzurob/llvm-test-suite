!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ statement
!*
!*  DESCRIPTION                :
!*           test round for complex(8) during READ.
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!* ===================================================================

  program roundX8ReadEdit01

    implicit none

    character(18) :: r_mode
    complex(8) rd

    integer, parameter::unit_r = 2

    rd = (0.0D0, 0.0D0)

    ! round in up mode

    open(unit_r, file='roundX8ReadEdit01.dat', action='read', round="up")

    read(unit_r, '(2f15.13)', round="up") rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'UP') error stop 1_4

    if(transfer(dreal(rd), 0_8) .ne. 4608308547941528973_8) error stop 2_4

    if(transfer(dimag(rd), 0_8) .ne. -4615063488913246836_8) error stop 3_4

    close(unit_r)

    ! round in down mode

    open(unit_r, file='roundX8ReadEdit01.dat', action='read', round="down")

    read(unit_r, '(2f15.13)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'DOWN  ') error stop 4_4

    if(transfer(dreal(rd), 0_8) .ne. 4608308547941528972_8) error stop 5_4

    if(transfer(dimag(rd),0_8) .ne. -4615063488913246835_8) error stop 6_4

    close(unit_r)

   ! round in default mode. Should be processor_defined, not in DOWN mode
   ! since unit_r is closed by previous statement.

    open(unit_r, file='roundX8ReadEdit01.dat', action='read')

    read(unit_r, '(2f15.13)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED  ') error stop 7_4

    if(transfer(dreal(rd),0_8) .ne. 4608308547941528972_8) error stop 8_4

    if(transfer(dimag(rd),0_8) .ne. -4615063488913246836_8) error stop 9_4

    close(unit_r)

    ! round in zero mode

    open(unit_r, file='roundX8ReadEdit01.dat', action='read', round="zero")

    read(unit_r, '(2f15.13)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'ZERO  ') error stop 10_4

    if(transfer(dreal(rd),0_8) .ne. 4608308547941528972_8) error stop 11_4

    if(transfer(dimag(rd),0_8) .ne. -4615063488913246836_8) error stop 12_4

    close(unit_r)

    ! round in nearest mode

    open(unit_r, file='roundX8ReadEdit01.dat', action='read', round="nearest")

    read(unit_r, '(2f15.13)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'NEAREST  ') error stop 13_4

    if(transfer(dreal(rd),0_8) .ne. 4608308547941528972_8) error stop 14_4

    if(transfer(dimag(rd),0_8) .ne. -4615063488913246836_8) error stop 15_4

    close(unit_r)

    ! round in processor_defined mode

    open(unit_r, file='roundX8ReadEdit01.dat', action='read',      &
       & round="processor_defined")

    read(unit_r, '(2f15.13)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED  ') error stop 16_4

    if(transfer(dreal(rd),0_8) .ne. 4608308547941528972_8) error stop 17_4

    if(transfer(dimag(rd),0_8) .ne. -4615063488913246836_8) error stop 18_4

    close(unit_r)

    ! round in compatible mode

    open(unit_r, file='roundX8ReadEdit01.dat', action='read',      &
      & round="compatible")

    read(unit_r, '(2f15.13)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') error stop 19_4

    if(transfer(dreal(rd),0_8) .ne. 4608308547941528972_8) error stop 20_4

    if(transfer(dimag(rd),0_8) .ne. -4615063488913246836_8) error stop 21_4

    close(unit_r)

  end program roundX8ReadEdit01
