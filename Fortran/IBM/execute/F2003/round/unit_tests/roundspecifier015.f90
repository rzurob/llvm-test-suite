!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of ROUND= specifier in
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


  write(buffer,FMT='(f8.5)', ROUND='up') write_num1
  print*,buffer
  read(read_buf1,FMT='(f8.6)', ROUND='up') read_num1
  if (read_num1 .ne. z'40CE7D14') error stop 1

  write(buffer,FMT='(f8.5)', ROUND='up') write_num2
  print *, buffer
  read(read_buf2,FMT='(f8.6)', ROUND='up') read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 2

  write(buffer,FMT='(f9.5)', ROUND='up') write_num3
  print *, buffer
  read(read_buf3,FMT='(f9.6)', ROUND='up') read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 3

  write(buffer,FMT='(f9.5)', ROUND='up') write_num4
  print *, buffer
  read(read_buf4,FMT='(f9.6)', ROUND='up') read_num4
  if (read_num4 .ne. z'C0CE7D06') error stop 4

  write(buffer,FMT='(f8.5)', ROUND='down') write_num1
  print *, buffer
  read(read_buf1,FMT='(f8.6)', ROUND='down') read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 5

  write(buffer,FMT='(f8.5)', ROUND='down') write_num2
  print *, buffer
  read(read_buf2,FMT='(f8.6)', ROUND='down') read_num2
  if (read_num2 .ne. z'40CE7D06') error stop 6

  write(buffer,FMT='(f9.5)', ROUND='down') write_num3
  print *, buffer
  read(read_buf3,FMT='(f9.6)', ROUND='down') read_num3
  if (read_num3 .ne. z'C0CE7D14') error stop 7

  write(buffer,FMT='(f9.5)', ROUND='down') write_num4
  print *, buffer
  read(read_buf4,FMT='(f9.6)', ROUND='down') read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 8

  write(buffer,FMT='(f8.5)', ROUND='zero') write_num1
  print *, buffer
  read(read_buf1,FMT='(f8.6)', ROUND='zero') read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 9

  write(buffer,FMT='(f8.5)', ROUND='zero') write_num2
  print *, buffer
  read(read_buf2,FMT='(f8.6)', ROUND='zero') read_num2
  if (read_num2 .ne. z'40CE7D06') error stop 10

  write(buffer,FMT='(f9.5)', ROUND='zero') write_num3
  print *, buffer
  read(read_buf3,FMT='(f9.6)', ROUND='zero') read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 11

  write(buffer,FMT='(f9.5)', ROUND='zero') write_num4
  print *, buffer
  read(read_buf4,FMT='(f9.6)', ROUND='zero') read_num4
  if (read_num4 .ne. z'C0CE7D06') error stop 12

  write(buffer,FMT='(f8.5)', ROUND='nearest') write_num1
  print *, buffer
  read(read_buf1,FMT='(f8.6)', ROUND='nearest') read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 13

  write(buffer,FMT='(f8.5)', ROUND='nearest') write_num2
  print *, buffer
  read(read_buf2,FMT='(f8.6)', ROUND='nearest') read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 14

  write(buffer,FMT='(f9.5)', ROUND='nearest') write_num3
  print *, buffer
  read(read_buf3,FMT='(f9.6)', ROUND='nearest') read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 15

  write(buffer,FMT='(f9.5)', ROUND='nearest') write_num4
  print *, buffer
  read(read_buf4,FMT='(f9.6)', ROUND='nearest') read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 16

  write(buffer,FMT='(f8.5)', ROUND='compatible') write_num1
  print *, buffer
  read(read_buf1,FMT='(f8.6)', ROUND='compatible') read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 17

  write(buffer,FMT='(f8.5)', ROUND='compatible') write_num2
  print *, buffer
  read(read_buf2,FMT='(f8.6)', ROUND='compatible') read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 18

  write(buffer,FMT='(f9.5)', ROUND='compatible') write_num3
  print *, buffer
  read(read_buf3,FMT='(f9.6)', ROUND='compatible') read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 19

  write(buffer,FMT='(f9.5)', ROUND='compatible') write_num4
  print *, buffer
  read(read_buf4,FMT='(f9.6)', ROUND='compatible') read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 20

  write(buffer,FMT='(f8.5)', ROUND='processor_defined') write_num1
  print *, buffer
  read(read_buf1,FMT='(f8.6)', ROUND='processor_defined') read_num1
  if (read_num1 .ne. z'40CE7D13') error stop 21

  write(buffer,FMT='(f8.5)', ROUND='processor_defined') write_num2
  print *, buffer
  read(read_buf2,FMT='(f8.6)', ROUND='processor_defined') read_num2
  if (read_num2 .ne. z'40CE7D07') error stop 22

  write(buffer,FMT='(f9.5)', ROUND='processor_defined') write_num3
  print *, buffer
  read(read_buf3,FMT='(f9.6)', ROUND='processor_defined') read_num3
  if (read_num3 .ne. z'C0CE7D13') error stop 23

  write(buffer,FMT='(f9.5)', ROUND='processor_defined') write_num4
  print *, buffer
  read(read_buf4,FMT='(f9.6)', ROUND='processor_defined') read_num4
  if (read_num4 .ne. z'C0CE7D07') error stop 24
end
