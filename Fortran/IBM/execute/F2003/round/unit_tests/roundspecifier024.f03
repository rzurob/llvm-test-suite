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
!*  DESCRIPTION                : functional testing of ROUND=specifier in
!*                               READ, WRITE statements with real(16)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real(16) :: write_num1,write_num2,write_num3, write_num4
  real(16) :: read_num1, read_num2, read_num3, read_num4

  character(40) :: round_mode


  write_num1=234.5Q0
  write_num2=237.5Q0
  write_num3=-234.5Q0
  write_num4=-237.5Q0

  open(2, file='roundspecifier024.out', ROUND='up')
  open(3, file='real16.dat', ROUND='down')

  write(UNIT=2,FMT='(f4.0)', ROUND='up') write_num1
  read(UNIT=3,FMT='(f30.28)',ROUND='up' ) read_num1
  if (read_num1 .ne. z'4019CFA84384E97EBCC9A114DE4CF5AB') error stop 1

  write(UNIT=2,FMT='(f4.0)', ROUND='up') write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND='up') read_num2
  if (read_num2 .ne. z'4019CFA84384E97EBCC9A114DE4D0D70') error stop 2

  write(UNIT=2,FMT='(f5.0)', ROUND='up') write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND='up') read_num3
  if (read_num3 .ne. z'C019CFA84384E97E3CC9A114DE4CF5AB') error stop 3

  write(UNIT=2,FMT='(f5.0)', ROUND='up') write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND='up') read_num4
  if (read_num4 .ne. z'C019CFA84384E97E3CC9A114DE4D0D70') error stop 4

  rewind 3

  write(UNIT=2,FMT='(f4.0)', ROUND='down') write_num1
  read(UNIT=3,FMT='(f30.28)', ROUND='down') read_num1
  if (read_num1 .ne. z'4019CFA84384E97D3CA97BAC86CC2950') error stop 5

  write(UNIT=2,FMT='(f4.0)', ROUND='down') write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND='down') read_num2
  if (read_num2 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D') error stop 6

  write(UNIT=2,FMT='(f5.0)', ROUND='down') write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND='down') read_num3
  if (read_num3 .ne. z'C019CFA84384E97DBCA97BAC86CC2950') error stop 7

  write(UNIT=2,FMT='(f5.0)', ROUND='down') write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND='down') read_num4
  if (read_num4 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D') error stop 8

  rewind 3

  write(UNIT=2,FMT='(f4.0)', ROUND='zero') write_num1
  read(UNIT=3,FMT='(f30.28)', ROUND='zero') read_num1
  if (read_num1 .ne. z'4019CFA84384E97D3CA97BAC86CC2950') error stop 9

  write(UNIT=2,FMT='(f4.0)', ROUND='zero') write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND='zero') read_num2
  if (read_num2 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D') error stop 10

  write(UNIT=2,FMT='(f5.0)', ROUND='zero') write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND='zero') read_num3
  if (read_num3 .ne. z'C019CFA84384E97DBCA97BAC86CC2950') error stop 11

  write(UNIT=2,FMT='(f5.0)', ROUND='zero') write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND='zero') read_num4
  if (read_num4 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D') error stop 12

  rewind 3

  write(UNIT=2,FMT='(f4.0)', ROUND='nearest') write_num1
  read(UNIT=3,FMT='(f30.28)', ROUND='nearest') read_num1
  if (read_num1 .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 13

  write(UNIT=2,FMT='(f4.0)', ROUND='nearest') write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND='nearest') read_num2
  if (read_num2 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 14

  write(UNIT=2,FMT='(f5.0)', ROUND='nearest') write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND='nearest') read_num3
  if (read_num3 .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 15

  write(UNIT=2,FMT='(f5.0)', ROUND='nearest') write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND='nearest') read_num4
  if (read_num4 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 16

  rewind 3

  write(UNIT=2,FMT='(f4.0)', ROUND='compatible') write_num1
  read(UNIT=3,FMT='(f30.28)', ROUND='compatible') read_num1
  if (read_num1 .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 17

  write(UNIT=2,FMT='(f4.0)', ROUND='compatible') write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND='compatible') read_num2
  if (read_num2 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 19

  write(UNIT=2,FMT='(f5.0)', ROUND='compatible') write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND='compatible') read_num3
  if (read_num3 .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 19

  write(UNIT=2,FMT='(f5.0)', ROUND='compatible') write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND='compatible') read_num4
  if (read_num4 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 20

  rewind 3

  write(UNIT=2,FMT='(f4.0)', ROUND='processor_defined') write_num1
  read(UNIT=3,FMT='(f30.28)', ROUND='processor_defined') read_num1
  if (read_num1 .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 21

  write(UNIT=2,FMT='(f4.0)', ROUND='processor_defined') write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND='processor_defined') read_num2
  if (read_num2 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 22

  write(UNIT=2,FMT='(f5.0)', ROUND='processor_defined') write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND='processor_defined') read_num3
  if (read_num3 .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 23

  write(UNIT=2,FMT='(f5.0)', ROUND='processor_defined') write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND='processor_defined') read_num4
  if (read_num4 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 24

  rewind 3

  round_mode='up'
  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num1
  if (read_num1 .ne. z'4019CFA84384E97EBCC9A114DE4CF5AB') error stop 25

  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num2
  if (read_num2 .ne. z'4019CFA84384E97EBCC9A114DE4D0D70') error stop 26

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num3
  if (read_num3 .ne. z'C019CFA84384E97E3CC9A114DE4CF5AB') error stop 27

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num4
  if (read_num4 .ne. z'C019CFA84384E97E3CC9A114DE4D0D70') error stop 28

  rewind 3

  round_mode='down'
  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num1
  if (read_num1 .ne. z'4019CFA84384E97D3CA97BAC86CC2950') error stop 29

  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num2
  if (read_num2 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D') error stop 30

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num3
  if (read_num3 .ne. z'C019CFA84384E97DBCA97BAC86CC2950') error stop 31

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num4
  if (read_num4 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D') error stop 32

  rewind 3

  round_mode='zero'
  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num1
  if (read_num1 .ne. z'4019CFA84384E97D3CA97BAC86CC2950') error stop 33

  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num2
  if (read_num2 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D') error stop 34

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num3
  if (read_num3 .ne. z'C019CFA84384E97DBCA97BAC86CC2950') error stop 35

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num4
  if (read_num4 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D') error stop 36

  rewind 3

  round_mode='nearest'
  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num1
  if (read_num1 .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 37

  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num2
  if (read_num2 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 38

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num3
  if (read_num3 .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 39

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num4
  if (read_num4 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 40

  rewind 3

  round_mode='compatible'
  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num1
  if (read_num1 .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 41

  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num2
  if (read_num2 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 42

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num3
  if (read_num3 .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 43

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num4
  if (read_num4 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 44

  rewind 3

  round_mode='processor_defined'
  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num1
  if (read_num1 .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 45

  write(UNIT=2,FMT='(f4.0)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(f30.28)', ROUND=round_mode) read_num2
  if (read_num2 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 46

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num3
  if (read_num3 .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 46

  write(UNIT=2,FMT='(f5.0)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(f31.28)', ROUND=round_mode) read_num4
  if (read_num4 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 48
 end
