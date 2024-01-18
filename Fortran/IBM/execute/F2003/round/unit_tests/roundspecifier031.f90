!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier031.f
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

  complex(16) :: write_num1,write_num2,write_num3, write_num4
  complex(16) :: read_num1, read_num2, read_num3, read_num4
  character(70) :: round_mode_read, round_mode_write



  write_num1=(6.4527903127549254938759383268,6.4527903127549254938759383268)
  write_num2=(6.4527903127549254938759383262,6.4527903127549254938759383262)
  write_num3=(-6.4527903127549254938759383268,-6.4527903127549254938759383268)
  write_num4=(-6.4527903127549254938759383262,-6.4527903127549254938759383262)

  !read formatting
  10 format(ru,2f31.28)
  20 format(ru,2f32.28)
  30 format(rd,2f31.28)
  40 format(rd,2f32.28)
  50 format(rz,2f31.28)
  60 format(rz,2f32.28)
  70 format(rn,2f31.28)
  80 format(rn,2f32.28)
  90 format(rc,2f31.28)
  100 format(rc,2f32.28)
  110 format(rp,2f31.28)
  120 format(rp,2f32.28)

  !write formatting
  130 format(ru,2f22.18)
  140 format(ru,2f23.18)
  150 format(rd,2f22.18)
  160 format(rd,2f23.18)
  170 format(rz,2f22.18)
  180 format(rz,2f23.18)
  190 format(rn,2f22.18)
  200 format(rn,2f23.18)
  210 format(rc,2f22.18)
  220 format(rc,2f23.18)
  230 format(rp,2f22.18)
  240 format(rp,2f23.18)

  open(2, file='roundspecifier031.out', ROUND='up')
  open(3, file='complex16.dat', ROUND='down')

  write(2,130) write_num1
  read(3,10) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97EBCC9A114DE4CF5AB' .or. aimag(read_num1) .ne. z'4019CFA84384E97EBCC9A114DE4CF5AB') error stop 1

  write(2,130) write_num2
  read(3,10) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97EBCC9A114DE4D0D70' .or. aimag(read_num2) .ne. z'4019CFA84384E97EBCC9A114DE4D0D70') error stop 2

  write(2,140) write_num3
  read(3,20) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97E3CC9A114DE4CF5AB' .or. aimag(read_num3) .ne. z'C019CFA84384E97E3CC9A114DE4CF5AB') error stop 3

  write(2,140) write_num4
  read(3,20) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97E3CC9A114DE4D0D70' .or. aimag(read_num4) .ne. z'C019CFA84384E97E3CC9A114DE4D0D70') error stop 4

  rewind 3

  write(2,150) write_num1
  read(3,30) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2950' .or. aimag(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2950') error stop 5

  write(2,150) write_num2
  read(3,30) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D' .or. aimag(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D') error stop 6

  write(2,160) write_num3
  read(3,40) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2950' .or. aimag(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2950') error stop 7

  write(2,160) write_num4
  read(3,40) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D' .or. aimag(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D') error stop 8

  rewind 3

  write(2,170) write_num1
  read(3,50) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2950' .or. aimag(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2950') error stop 9

  write(2,170) write_num2
  read(3,50) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D' .or. aimag(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D') error stop 10

  write(2,180) write_num3
  read(3,60) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2950' .or. aimag(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2950') error stop 11

  write(2,180) write_num4
  read(3,60) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D' .or. aimag(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D') error stop 12

  rewind 3

  write(2,190) write_num1
  read(3,70) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951' .or. aimag(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 13

  write(2,190) write_num2
  read(3,70) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' .or. aimag(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 14

  write(2,200) write_num3
  read(3,80) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951' .or. aimag(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 15

  write(2,200) write_num4
  read(3,80) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' .or. aimag(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 16

  rewind 3

  write(2,210) write_num1
  read(3,90) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951' .or. aimag(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 17

  write(2,210) write_num2
  read(3,90) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' .or. aimag(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 18

  write(2,220) write_num3
  read(3,100) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951' .or. aimag(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 19

  write(2,220) write_num4
  read(3,100) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' .or. aimag(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 20

  rewind 3

  write(2,230) write_num1
  read(3,110) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951' .or. aimag(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 21

  write(2,230) write_num2
  read(3,110) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' .or. aimag(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 22

  write(2,240) write_num3
  read(3,120) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951' .or. aimag(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 23

  write(2,240) write_num4
  read(3,120) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' .or. aimag(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 24

  rewind 3

  round_mode_read='(ru,2f31.28)'
  round_mode_write='(ru,2f22.18)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97EBCC9A114DE4CF5AB' .or. aimag(read_num1) .ne. z'4019CFA84384E97EBCC9A114DE4CF5AB') error stop 25

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97EBCC9A114DE4D0D70' .or. aimag(read_num2) .ne. z'4019CFA84384E97EBCC9A114DE4D0D70') error stop 26

  round_mode_read='(ru,2f32.28)'
  round_mode_write='(ru,2f23.18)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97E3CC9A114DE4CF5AB' .or. aimag(read_num3) .ne. z'C019CFA84384E97E3CC9A114DE4CF5AB') error stop 27

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97E3CC9A114DE4D0D70' .or. aimag(read_num4) .ne. z'C019CFA84384E97E3CC9A114DE4D0D70') error stop 28

  rewind 3

  round_mode_read='(rd,2f31.28)'
  round_mode_write='(rd,2f22.18)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2950' .or. aimag(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2950') error stop 29

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D' .or. aimag(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D') error stop 30

  round_mode_read='(rd,2f32.28)'
  round_mode_write='(rd,2f23.18)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2950' .or. aimag(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2950') error stop 31

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D' .or. aimag(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D') error stop 32

  rewind 3

  round_mode_read='(rz,2f31.28)'
  round_mode_write='(rz,2f22.18)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2950' .or. aimag(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2950') error stop 33

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D' .or. aimag(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D') error stop 34

  round_mode_read='(rz,2f32.28)'
  round_mode_write='(rz,2f23.18)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2950' .or. aimag(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2950') error stop 35

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D' .or. aimag(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D') error stop 36

  rewind 3

  round_mode_read='(rn,2f31.28)'
  round_mode_write='(rn,2f22.18)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951' .or. aimag(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 37

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' .or. aimag(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 38

  round_mode_read='(rn,2f32.28)'
  round_mode_write='(rn,2f23.18)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951' .or. aimag(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 39

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' .or. aimag(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 40

  rewind 3

  round_mode_read='(rc,2f31.28)'
  round_mode_write='(rc,2f22.18)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951' .or. aimag(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 41

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' .or. aimag(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 42

  round_mode_read='(rc,2f32.28)'
  round_mode_write='(rc,2f23.18)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951' .or. aimag(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 43

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' .or. aimag(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 44

  rewind 3

  round_mode_read='(rp,2f31.28)'
  round_mode_write='(rp,2f22.18)'
  write(2,round_mode_write) write_num1
  read(3,round_mode_read) read_num1
  if (real(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951' .or. aimag(read_num1) .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 45

  write(2,round_mode_write) write_num2
  read(3,round_mode_read) read_num2
  if (real(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' .or. aimag(read_num2) .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 46

  round_mode_read='(rp,2f32.28)'
  round_mode_write='(rp,2f23.18)'
  write(2,round_mode_write) write_num3
  read(3,round_mode_read) read_num3
  if (real(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951' .or. aimag(read_num3) .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 46

  write(2,round_mode_write) write_num4
  read(3,round_mode_read) read_num4
  if (real(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' .or. aimag(read_num4) .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 48

end
