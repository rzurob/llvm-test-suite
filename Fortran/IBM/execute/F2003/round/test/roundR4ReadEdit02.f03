!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ statement
!*
!*  DESCRIPTION                :
!*           test ROUND mode priority as specified simutanuously in
!*           OPEN statmenet, as specifier in READ statement and as
!*           edit descriptor is READ statement.
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!* ===================================================================

  program roundR4ReadEdit02

    implicit none

    character(18) :: r_mode
    real rd1, rd2

    integer, parameter::unit_r = 2

    rd1 = 0.0
    rd2 = 0.0

    ! round in up mode

    open(unit_r, file='roundR4ReadEdit02.dat', action='read', round="down")

    read(unit_r, '(RU, e6.5, es7.5)', round="zero") rd1, rd2

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'DOWN') error stop 1_4

    if(transfer(rd1, 0) .ne. 1067450788) error stop 2_4

    if(transfer(rd2, 0) .ne. -1080032861) error stop 3_4

    close(unit_r)

    ! round in down mode

    open(unit_r, file='roundR4ReadEdit02.dat', action='read', round="zero")

    read(unit_r, '(RD, en6.5, d7.5)', round="up") rd1, rd2

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'ZERO  ') error stop 4_4

    if(transfer(rd1, 0) .ne. 1067450787) error stop 5_4

    if(transfer(rd2, 0) .ne. -1080032860) error stop 6_4

    close(unit_r)

    ! round in zero mode

    open(unit_r, file='roundR4ReadEdit02.dat', action='read', round="up")

    read(unit_r, '(RZ, g6.5, g7.5)', round="up") rd1, rd2

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'UP  ') error stop 7_4

    if(transfer(rd1, 0) .ne. 1067450787) error stop 8_4

    if(transfer(rd2, 0) .ne. -1080032861) error stop 9_4

    close(unit_r)

    ! round in nearest mode

    open(unit_r, file='roundR4ReadEdit02.dat', action='read', round="down")

    read(unit_r, '(RN,e6.5, f7.5)', round="up") rd1, rd2

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'DOWN  ') error stop 10_4

    if(transfer(rd1, 0) .ne. 1067450787) error stop 11_4

    if(transfer(rd2, 0) .ne. -1080032861) error stop 12_4

    close(unit_r)

    ! round in processor_defined mode

    open(unit_r, file='roundR4ReadEdit02.dat', action='read',             &
    round="up")

    read(unit_r, '(RP, en6.5, es7.5)', round="down") rd1, rd2

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'UP  ') error stop 13_4

    if(transfer(rd1, 0) .ne. 1067450787) error stop 14_4

    if(transfer(rd2, 0) .ne. -1080032861) error stop 15_4

    close(unit_r)

    ! round in compatible mode

    open(unit_r, file='roundR4ReadEdit02.dat', action='read')

    read(unit_r, '(RC, f6.5, g7.5)', round="up") rd1, rd2

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') error stop 16_4

    if(transfer(rd1, 0) .ne. 1067450787) error stop 17_4

    if(transfer(rd2, 0) .ne. -1080032861) error stop 18_4

    close(unit_r)

  end program roundR4ReadEdit02
