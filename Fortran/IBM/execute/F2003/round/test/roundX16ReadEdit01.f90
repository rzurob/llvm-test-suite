!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ statement
!*
!*  DESCRIPTION                :
!*           test round with complex(16) in read.
!*           10.6.1.2.6. The rounding mode can be specified by a data
!*           transfer input/output statement, an OPEN statement or an
!*           edit descriptor.
!* ===================================================================

  program roundX16ReadEdit01

    implicit none

    character(18) :: r_mode
    complex(16) rd

    integer, parameter::unit_r = 2

    rd = (0.0Q0, 0.0Q0)

    ! round in up mode

    open(unit_r, file='roundX16ReadEdit01.dat', action='read', round="up")

    read(unit_r, '(2f26.24)', round="up") rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'UP') error stop 1_4

    if(qreal(rd) .ne. z'3FF400355F73E1C2BC97B092CC929AFF') error stop 2_4

    if(qimag(rd) .ne. z'BFF400355F73E1C23C97B092CC929AFF') error stop 3_4

    close(unit_r)

    ! round in down mode

    open(unit_r, file='roundX16ReadEdit01.dat', action='read', round="down")

    read(unit_r, '(2f26.24)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'DOWN  ') error stop 4_4

    if(qreal(rd) .ne. z'3FF400355F73E1C13CA427B699B6B27F') error stop 5_4

    if(qimag(rd) .ne. z'BFF400355F73E1C1BCA427B699B6B27F') error stop 6_4

    close(unit_r)

    ! round in zero mode

    open(unit_r, file='roundX16ReadEdit01.dat', action='read', round="zero")

    read(unit_r, '(2f26.24)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'ZERO  ') error stop 7_4

    if(qreal(rd) .ne. z'3FF400355F73E1C13CA427B699B6B27F') error stop 8_4

    if(qimag(rd) .ne. z'BFF400355F73E1C1BCA427B699B6B27F') error stop 9_4

    close(unit_r)

    ! round in nearest mode

    open(unit_r, file='roundX16ReadEdit01.dat', action='read', round="nearest")

    read(unit_r, '(2f26.24)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'NEAREST  ') error stop 10_4

    if(qreal(rd) .ne. z'3FF400355F73E1C2BC97B092CC929B00') error stop 11_4

    if(qimag(rd) .ne. z'BFF400355F73E1C23C97B092CC929B00') error stop 12_4

    close(unit_r)

    ! round in processor_defined mode

    open(unit_r, file='roundX16ReadEdit01.dat', action='read',          &
       & round="processor_defined")

    read(unit_r, '(2f26.24)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED  ') error stop 13_4

    if(qreal(rd) .ne. z'3FF400355F73E1C2BC97B092CC929B00') error stop 14_4

    if(qimag(rd) .ne. z'BFF400355F73E1C23C97B092CC929B00') error stop 15_4

    close(unit_r)

    ! round in compatible mode

    open(unit_r, file='roundX16ReadEdit01.dat', action='read',           &
      & round="compatible")

    read(unit_r, '(2f26.24)') rd

    inquire(unit_r, round=r_mode)

    if(r_mode .ne. 'COMPATIBLE') error stop 16_4

    if(qreal(rd) .ne. z'3FF400355F73E1C2BC97B092CC929B00') error stop 17_4

    if(qimag(rd) .ne. z'BFF400355F73E1C23C97B092CC929B00') error stop 18_4

    close(unit_r)

  end program roundX16ReadEdit01
