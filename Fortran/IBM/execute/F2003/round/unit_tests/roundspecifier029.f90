!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier029.f
! %VERifY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of round edit descriptor in
!*                               READ, WRITE statements with
!*                               es edit descriptor
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  complex(8) :: write_num1,write_num2,write_num3, write_num4
  complex(8) :: read_num1, read_num2, read_num3, read_num4
  character(50) :: round_mode_read, round_mode_write

  write_num1=(6.45279031275468,6.45279031275468)
  write_num2=(6.45279031275462,6.45279031275462)
  write_num3=(-6.45279031275468,-6.45279031275468)
  write_num4=(-6.45279031275462,-6.45279031275462)

  !read formatting
  10 format(ru,2f17.14)
  20 format(ru,2f18.14)
  30 format(rd,2f17.14)
  40 format(rd,2f18.14)
  50 format(rz,2f17.14)
  60 format(rz,2f18.14)
  70 format(rn,2f17.14)
  80 format(rn,2f18.14)
  90 format(rc,2f17.14)
  100 format(rc,2f18.14)
  110 format(rp,2f17.14)
  120 format(rp,2f18.14)

  !write formatting
  130 format(ru,2f17.13)
  140 format(ru,2f18.13)
  150 format(rd,2f17.13)
  160 format(rd,2f18.13)
  170 format(rz,2f17.13)
  180 format(rz,2f18.13)
  190 format(rn,2f17.13)
  200 format(rn,2f18.13)
  210 format(rc,2f17.13)
  220 format(rc,2f18.13)
  230 format(rp,2f17.13)
  240 format(rp,2f18.13)

  open(2, file='roundspecifier029.out', ROUND='up')
  open(3, file='complex8.dat', ROUND='down')

  write(2,130) write_num1
  read(3,10) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E869' .or. aimag(read_num1) .ne. z'4019CFA84384E869') error stop 1

  write(2,130) write_num2
  read(3,10) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E826' .or. aimag(read_num2) .ne. z'4019CFA84384E826') error stop 2

  write(2,140) write_num3
  read(3,20) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E868' .or. aimag(read_num3) .ne. z'C019CFA84384E868') error stop 3

  write(2,140) write_num4
  read(3,20) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E825' .or. aimag(read_num4) .ne. z'C019CFA84384E825') error stop 4

  rewind 3

  write(2,150) write_num1
  read(3,30) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E868' .or. aimag(read_num1) .ne. z'4019CFA84384E868') error stop 5

  write(2,150) write_num2
  read(3,30) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E825' .or. aimag(read_num2) .ne. z'4019CFA84384E825') error stop 6

  write(2,160) write_num3
  read(3,40) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E869' .or. aimag(read_num3) .ne. z'C019CFA84384E869') error stop 7

  write(2,160) write_num4
  read(3,40) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E826' .or. aimag(read_num4) .ne. z'C019CFA84384E826') error stop 8

  rewind 3

  write(2,170) write_num1
  read(3,50) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E868' .or. aimag(read_num1) .ne. z'4019CFA84384E868') error stop 9

  write(2,170) write_num2
  read(3,50) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E825' .or. aimag(read_num2) .ne. z'4019CFA84384E825') error stop 10

  write(2,180) write_num3
  read(3,60) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E868' .or. aimag(read_num3) .ne. z'C019CFA84384E868') error stop 11

  write(2,180) write_num4
  read(3,60) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E825' .or. aimag(read_num4) .ne. z'C019CFA84384E825') error stop 12

  rewind 3

  write(2,190) write_num1
  read(3,70) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E869' .or. aimag(read_num1) .ne. z'4019CFA84384E869') error stop 13

  write(2,190) write_num2
  read(3,70) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E825' .or. aimag(read_num2) .ne. z'4019CFA84384E825') error stop 14

  write(2,200) write_num3
  read(3,80) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E869' .or. aimag(read_num3) .ne. z'C019CFA84384E869') error stop 15

  write(2,200) write_num4
  read(3,80) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E825' .or. aimag(read_num4) .ne. z'C019CFA84384E825') error stop 16

  rewind 3

  write(2,210) write_num1
  read(3,90) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E869' .or. aimag(read_num1) .ne. z'4019CFA84384E869') error stop 17

  write(2,210) write_num2
  read(3,90) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E825' .or. aimag(read_num2) .ne. z'4019CFA84384E825') error stop 18

  write(2,220) write_num3
  read(3,100) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E869' .or. aimag(read_num3) .ne. z'C019CFA84384E869') error stop 19

  write(2,220) write_num4
  read(3,100) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E825' .or. aimag(read_num4) .ne. z'C019CFA84384E825') error stop 20

  rewind 3

  write(2,230) write_num1
  read(3,110) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E869' .or. aimag(read_num1) .ne. z'4019CFA84384E869') error stop 21

  write(2,230) write_num2
  read(3,110) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E825' .or. aimag(read_num2) .ne. z'4019CFA84384E825') error stop 22

  write(2,240) write_num3
  read(3,120) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E869' .or. aimag(read_num3) .ne. z'C019CFA84384E869') error stop 23

  write(2,240) write_num4
  read(3,120) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E825' .or. aimag(read_num4) .ne. z'C019CFA84384E825') error stop 24

  rewind 3

  round_mode_read='(ru,2f17.14)'
  round_mode_write='(ru,2f17.13)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E869' .or. aimag(read_num1) .ne. z'4019CFA84384E869') error stop 25

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E826' .or. aimag(read_num2) .ne. z'4019CFA84384E826') error stop 26

  round_mode_read='(ru,2f18.14)'
  round_mode_write='(ru,2f18.13)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E868' .or. aimag(read_num3) .ne. z'C019CFA84384E868') error stop 27

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E825' .or. aimag(read_num4) .ne. z'C019CFA84384E825') error stop 28

  rewind 3

  round_mode_read='(rd,2f17.14)'
  round_mode_write='(rd,2f17.13)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E868' .or. aimag(read_num1) .ne. z'4019CFA84384E868') error stop 29

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E825' .or. aimag(read_num2) .ne. z'4019CFA84384E825') error stop 30

  round_mode_read='(rd,2f18.14)'
  round_mode_write='(rd,2f18.13)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E869' .or. aimag(read_num3) .ne. z'C019CFA84384E869') error stop 31

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E826' .or. aimag(read_num4) .ne. z'C019CFA84384E826') error stop 32

  rewind 3

  round_mode_read='(rz,2f17.14)'
  round_mode_write='(rz,2f17.13)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E868' .or. aimag(read_num1) .ne. z'4019CFA84384E868') error stop 33

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E825' .or. aimag(read_num2) .ne. z'4019CFA84384E825') error stop 34

  round_mode_read='(rz,2f18.14)'
  round_mode_write='(rz,2f18.13)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E868' .or. aimag(read_num3) .ne. z'C019CFA84384E868') error stop 35

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E825' .or. aimag(read_num4) .ne. z'C019CFA84384E825') error stop 36

  rewind 3

  round_mode_read='(rn,2f17.14)'
  round_mode_write='(rn,2f17.13)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E869' .or. aimag(read_num1) .ne. z'4019CFA84384E869') error stop 37

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E825' .or. aimag(read_num2) .ne. z'4019CFA84384E825') error stop 38

  round_mode_read='(rn,2f18.14)'
  round_mode_write='(rn,2f18.13)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E869' .or. aimag(read_num3) .ne. z'C019CFA84384E869') error stop 39

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E825' .or. aimag(read_num4) .ne. z'C019CFA84384E825') error stop 40

  rewind 3

  round_mode_read='(rc,2f17.14)'
  round_mode_write='(rc,2f17.13)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E869' .or. aimag(read_num1) .ne. z'4019CFA84384E869') error stop 41

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E825' .or. aimag(read_num2) .ne. z'4019CFA84384E825') error stop 42

  round_mode_read='(rc,2f18.14)'
  round_mode_write='(rc,2f18.13)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E869' .or. aimag(read_num3) .ne. z'C019CFA84384E869') error stop 43

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E825' .or. aimag(read_num4) .ne. z'C019CFA84384E825') error stop 44

  rewind 3

  round_mode_read='(rp,2f17.14)'
  round_mode_write='(rp,2f17.13)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E869' .or. aimag(read_num1) .ne. z'4019CFA84384E869') error stop 45

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E825' .or. aimag(read_num2) .ne. z'4019CFA84384E825') error stop 46

  round_mode_read='(rp,2f18.14)'
  round_mode_write='(rp,2f18.13)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E869' .or. aimag(read_num3) .ne. z'C019CFA84384E869') error stop 46

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E825' .or. aimag(read_num4) .ne. z'C019CFA84384E825') error stop 48

end
