!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier018.f
! %VERIFY:
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
!*                               e edit descriptor
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: write_num1,write_num2,write_num3, write_num4
  real :: read_num1, read_num2, read_num3, read_num4
  character(20) :: round_mode_read, round_mode_write


  write_num1=6.452768
  write_num2=6.452762
  write_num3=-6.452768
  write_num4=-6.452762

  !read formatting
  10 format(ru,e13.7)
  20 format(ru,e14.7)
  30 format(rd,e13.7)
  40 format(rd,e14.7)
  50 format(rz,e13.7)
  60 format(rz,e14.7)
  70 format(rn,e13.7)
  80 format(rn,e14.7)
  90 format(rc,e13.7)
  100 format(rc,e14.7)
  110 format(rp,e13.7)
  120 format(rp,e14.7)

  !write formatting
  130 format(ru,e13.6)
  140 format(ru,e14.6)
  150 format(rd,e13.6)
  160 format(rd,e14.6)
  170 format(rz,e13.6)
  180 format(rz,e14.6)
  190 format(rn,e13.6)
  200 format(rn,e14.6)
  210 format(rc,e13.6)
  220 format(rc,e14.6)
  230 format(rp,e13.6)
  240 format(rp,e14.6)

  open(2, file='roundspecifier018.out', ROUND='up')
  open(3, file='real4.dat', ROUND='down')

  write(2,130) write_num1
  read(3,10) read_num1
  if (read_num1 .ne. z'40CE7D14') error stop 1

  write(2,130) write_num2
  read(3,10) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 2

  write(2,140) write_num3
  read(3,20) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 3

  write(2,140) write_num4
  read(3,20) read_num4
  if (read_num4 .ne. z'C0CE7D06') error stop 4

  rewind 3

  write(2,150) write_num1
  read(3,30) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 5

  write(2,150) write_num2
  read(3,30) read_num2
  if (read_num2 .ne. z'40CE7D06') error stop 6

  write(2,160) write_num3
  read(3,40) read_num3
  if (read_num3 .ne. z'C0CE7D14') error stop 7

  write(2,160) write_num4
  read(3,40) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 8

  rewind 3

  write(2,170) write_num1
  read(3,50) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 9

  write(2,170) write_num2
  read(3,50) read_num2
  if (read_num2 .ne. z'40CE7D06') error stop 10

  write(2,180) write_num3
  read(3,60) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 11

  write(2,180) write_num4
  read(3,60) read_num4
  if (read_num4 .ne. z'C0CE7D06') error stop 12

  rewind 3

  write(2,190) write_num1
  read(3,70) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 13

  write(2,190) write_num2
  read(3,70) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 14

  write(2,200) write_num3
  read(3,80) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 15

  write(2,200) write_num4
  read(3,80) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 16

  rewind 3

  write(2,210) write_num1
  read(3,90) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 17

  write(2,210) write_num2
  read(3,90) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 18

  write(2,220) write_num3
  read(3,100) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 19

  write(2,220) write_num4
  read(3,100) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 20

  rewind 3

  write(2,230) write_num1
  read(3,110) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 21

  write(2,230) write_num2
  read(3,110) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 22

  write(2,240) write_num3
  read(3,120) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 23

  write(2,240) write_num4
  read(3,120) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 24

  rewind 3

  round_mode_read='(ru,e13.7)'
  round_mode_write='(ru,e13.6)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (read_num1 .ne. z'40CE7D14') error stop 25

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 26

  round_mode_read='(ru,e14.7)'
  round_mode_write='(ru,e14.6)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 27

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (read_num4 .ne. z'C0CE7D06') error stop 28

  rewind 3

  round_mode_read='(rd,e13.7)'
  round_mode_write='(rd,e13.6)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 29

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (read_num2 .ne. z'40CE7D06') error stop 30

  round_mode_read='(rd,e14.7)'
  round_mode_write='(rd,e14.6)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (read_num3 .ne. z'C0CE7D14') error stop 31

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 32

  rewind 3

  round_mode_read='(rz,e13.7)'
  round_mode_write='(rz,e13.6)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 33

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (read_num2 .ne. z'40CE7D06') error stop 34

  round_mode_read='(rz,e14.7)'
  round_mode_write='(rz,e14.6)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 35

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (read_num4 .ne. z'C0CE7D06') error stop 36

  rewind 3

  round_mode_read='(rn,e13.7)'
  round_mode_write='(rn,e13.6)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 37

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 38

  round_mode_read='(rn,e14.7)'
  round_mode_write='(rn,e14.6)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 39

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 40

  rewind 3

  round_mode_read='(rc,e13.7)'
  round_mode_write='(rc,e13.6)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 41

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 42

  round_mode_read='(rc,e14.7)'
  round_mode_write='(rc,e14.6)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 43

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 44

  rewind 3

  round_mode_read='(rp,e13.7)'
  round_mode_write='(rp,e13.6)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 45

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 46

  round_mode_read='(rp,e14.7)'
  round_mode_write='(rp,e14.6)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 46

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 48

end

