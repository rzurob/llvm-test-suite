!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier016.f
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
!*  DESCRIPTION                : functional testing of round edit descriptors in
!*                               READ, WRITE statements with
!*                               internal files
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  character(10) :: buffer,read_buf1,read_buf2,read_buf3,read_buf4

  real :: write_num1,write_num2,write_num3, write_num4
  real :: read_num1, read_num2, read_num3, read_num4


  write_num1=6.452768
  write_num2=6.452762
  write_num3=-6.452768
  write_num4=-6.452762

  read_buf1="6.452768"
  read_buf2="6.452762"
  read_buf3="-6.452768"
  read_buf4="-6.452762"

  !read formating
  10 format(ru,f8.6)
  20 format(ru,f9.6)
  30 format(rd,f8.6)
  40 format(rd,f9.6)
  50 format(rz,f8.6)
  60 format(rz,f9.6)
  70 format(rn,f8.6)
  80 format(rn,f9.6)
  90 format(rc,f8.6)
  100 format(rc,f9.6)
  110 format(rp,f8.6)
  120 format(rp,f9.6)

  !write formating
  130 format(ru,f8.5)
  140 format(ru,f9.5)
  150 format(rd,f8.5)
  160 format(rd,f9.5)
  170 format(rz,f8.5)
  180 format(rz,f9.5)
  190 format(rn,f8.5)
  200 format(rn,f9.5)
  210 format(rc,f8.5)
  220 format(rc,f9.5)
  230 format(rp,f8.5)
  240 format(rp,f9.5)

  write(buffer,130) write_num1
  print *, buffer
  read(read_buf1,10) read_num1
  if (read_num1 .ne. z'40CE7D14') error stop 1

  write(buffer,130) write_num2
  print *, buffer
  read(read_buf2,10) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 2

  write(buffer,140) write_num3
  print *, buffer
  read(read_buf3,20) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 3

  write(buffer,140) write_num4
  print *, buffer
  read(read_buf4,20) read_num4
  if (read_num4 .ne. z'C0CE7D06') error stop 4

  write(buffer,150) write_num1
  print *, buffer
  read(read_buf1,30) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 5

  write(buffer,150) write_num2
  print *, buffer
  read(read_buf2,30) read_num2
  if (read_num2 .ne. z'40CE7D06') error stop 6

  write(buffer,160) write_num3
  print *, buffer
  read(read_buf3,40) read_num3
  if (read_num3 .ne. z'C0CE7D14') error stop 7

  write(buffer,160) write_num4
  print *, buffer
  read(read_buf4,40) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 8

  write(buffer,170) write_num1
  print *, buffer
  read(read_buf1,50) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 9

  write(buffer,170) write_num2
  print *, buffer
  read(read_buf2,50) read_num2
  if (read_num2 .ne. z'40CE7D06') error stop 10

  write(buffer,180) write_num3
  print *, buffer
  read(read_buf3,60) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 11

  write(buffer,180) write_num4
  print *, buffer
  read(read_buf4,60) read_num4
  if (read_num4 .ne. z'C0CE7D06') error stop 12

  write(buffer,190) write_num1
  print *, buffer
  read(read_buf1,70) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 13

  write(buffer,190) write_num2
  print *, buffer
  read(read_buf2,70) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 14

  write(buffer,200) write_num3
  print *, buffer
  read(read_buf3,80) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 15

  write(buffer,200) write_num4
  print *, buffer
  read(read_buf4,80) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 16

   write(buffer,210) write_num1
  print *, buffer
  read(read_buf1,90) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 17

  write(buffer,210) write_num2
  print *, buffer
  read(read_buf2,90) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 18

  write(buffer,220) write_num3
  print *, buffer
  read(read_buf3,100) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 19

  write(buffer,220) write_num4
  print *, buffer
  read(read_buf4,100) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 20

  write(buffer,230) write_num1
  print *, buffer
  read(read_buf1,110) read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 21

  write(buffer,230) write_num2
  print *, buffer
  read(read_buf2,110) read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 22

  write(buffer,240) write_num3
  print *, buffer
  read(read_buf3,120) read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 23

  write(buffer,240) write_num4
  print *, buffer
  read(read_buf4,120) read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 24

end
