!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ statement
!*
!*  DESCRIPTION                :
!*           test round for complex(4) during READ.
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!* ===================================================================

  program roundX4ReadEdit01

    implicit none

    character(18) :: r_mode
    complex rd

    integer, parameter::unit_r = 2
    logical(4), external :: precision_r4

    rd = (0.0, 0.0)

    ! round in up mode

    open(unit_r, file='roundX4ReadEdit01.dat', action='read', round="up")

    read(unit_r, '(2f7.5)', round="up") rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'UP') error stop 1_4

    if(transfer(real(rd), 0) .ne. 1067450788) error stop 2_4

    if(transfer(aimag(rd), 0) .ne. -1080032861) error stop 3_4

    close(unit_r)

    ! round in down mode

    open(unit_r, file='roundX4ReadEdit01.dat', action='read', round="down")

    read(unit_r, '(2f7.5)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'DOWN  ') error stop 4_4

    if(transfer(real(rd), 0) .ne. 1067450787) error stop 5_4

    if(transfer(aimag(rd), 0) .ne. -1080032860) error stop 6_4

    close(unit_r)

   ! round in default mode. Should be processor_defined, not in DOWN mode
   ! since unit_r is closed by previous statement.

    open(unit_r, file='roundX4ReadEdit01.dat', action='read')

    read(unit_r, '(2f7.5)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED  ') error stop 7_4

    if(transfer(real(rd),0) .ne. 1067450787) error stop 8_4

    if(transfer(aimag(rd),0) .ne. -1080032861) error stop 9_4

    close(unit_r)

    ! round in zero mode

    open(unit_r, file='roundX4ReadEdit01.dat', action='read', round="zero")

    read(unit_r, '(2f7.5)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'ZERO  ') error stop 10_4

    if(transfer(real(rd), 0) .ne. 1067450787) error stop 11_4

    if(transfer(aimag(rd), 0) .ne. -1080032861) error stop 12_4

    close(unit_r)

    ! round in nearest mode

    open(unit_r, file='roundX4ReadEdit01.dat', action='read', round="nearest")

    read(unit_r, '(2f7.5)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'NEAREST  ') error stop 13_4

    if(transfer(real(rd),0) .ne. 1067450787) error stop 14_4

    if(transfer(aimag(rd),0) .ne. -1080032861) error stop 15_4

    close(unit_r)

    ! round in processor_defined mode

    open(unit_r, file='roundX4ReadEdit01.dat', action='read',      &
       & round="processor_defined")

    read(unit_r, '(2f7.5)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED  ') error stop 16_4

    if(transfer(real(rd),0) .ne. 1067450787) error stop 17_4

    if(transfer(aimag(rd),0) .ne. -1080032861) error stop 18_4

    close(unit_r)

    ! round in compatible mode

    open(unit_r, file='roundX4ReadEdit01.dat', action='read',      &
      & round="compatible")

    read(unit_r, '(2f7.5)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') error stop 19_4

    if(.not. precision_r4(real(rd), real(transfer(4608308543886841401_8,1.0_8),4))) error stop 20_4

    if(.not. precision_r4(aimag(rd), real(transfer(-4615063492967934407_8,1.0_8),4))) error stop 21_4

    close(unit_r)

  end program roundX4ReadEdit01
