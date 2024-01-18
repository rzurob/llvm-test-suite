!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ statement
!*
!*  DESCRIPTION                :
!*           test round descriptor for complex(4) during READ.
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!* ===================================================================

  program roundX4ReadEdit02

    implicit none

    character(18) :: r_mode
    complex rd

    integer, parameter::unit_r = 2

    rd = (0.0, 0.0)

    ! round in up mode

    open(unit_r, file='roundX4ReadEdit02.dat', action='read')

    read(unit_r, '(RU, 2f7.5)', round="zero") rd

    if(transfer(real(rd), 0) .ne. 1067450788) error stop 1_4

    if(transfer(aimag(rd), 0) .ne. -1080032861) error stop 2_4

    rewind(unit_r)

    ! round in down mode

    read(unit_r, '(RD, 2f7.5)', round="up  ") rd

    if(transfer(real(rd), 0) .ne. 1067450787) error stop 3_4

    if(transfer(aimag(rd), 0) .ne. -1080032860) error stop 4_4

    rewind(unit_r)

    ! round in default mode

    read(unit_r, '(2f7.5)') rd

    if(transfer(real(rd),0) .ne. 1067450787) error stop 5_4

    if(transfer(aimag(rd),0) .ne. -1080032861) error stop 6_4

    rewind(unit_r)

    ! round in zero mode

    read(unit_r, '(RZ,2f7.5)') rd

    if(transfer(real(rd), 0) .ne. 1067450787) error stop 7_4

    if(transfer(aimag(rd), 0) .ne. -1080032861) error stop 8_4

    rewind(unit_r)

    ! round in nearest mode

    read(unit_r, '(RN,2f7.5)') rd

    if(transfer(real(rd),0) .ne. 1067450787) error stop 9_4

    if(transfer(aimag(rd),0) .ne. -1080032861) error stop 10_4

    rewind(unit_r)

    ! round in processor_defined mode

    read(unit_r, '(RP, 2f7.5)') rd

    if(transfer(real(rd),0) .ne. 1067450787) error stop 11_4

    if(transfer(aimag(rd),0) .ne. -1080032861) error stop 12_4

    rewind(unit_r)

    ! round in compatible mode

    read(unit_r, '(RC, 2f7.5)') rd

    if(transfer(real(rd),0) .ne. 1067450787) error stop 13_4

    if(transfer(aimag(rd),0) .ne. -1080032861) error stop 14_4

    close(unit_r)

  end program roundX4ReadEdit02
