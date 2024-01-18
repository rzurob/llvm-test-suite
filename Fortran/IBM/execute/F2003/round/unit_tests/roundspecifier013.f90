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
!*  DESCRIPTION                : functional testing of round edit descriptors in
!*                               READ statements
!*                               with descriptors set at compile and run time with
!*                               external files
!*
!234567890133456789013345678901334567890133456789013345678901334567890

  real :: num

  character(20) :: round_mode

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

  !Test edit descriptors with compile time encoding
  open(UNIT=2,FILE='real4.dat')

  read(2,10) num
  if (num .ne. z'40CE7D14') error stop 1

  read(2,10) num
  if (num .ne. z'40CE7D07' ) error stop 2

  read(2,20) num
  if (num .ne. z'C0CE7D13' ) error stop 3

  read(2,20) num
  if (num .ne. z'C0CE7D06' ) error stop 4

  close(2)

  !Test Read statement with rounding mode down
  open(UNIT=2,FILE='real4.dat')

  read(2,30) num
  if (num .ne. z'40CE7D13' ) error stop 5

  read(2,30) num
  if (num .ne. z'40CE7D06' ) error stop 6

  read(2,40) num
  if (num .ne. z'C0CE7D14' ) error stop 7

  read(2,40) num
  if (num .ne. z'C0CE7D07' ) error stop 8

  close(2)

  !Test Read statement with roundming mode zero
  open(UNIT=2,FILE='real4.dat')

  read(2,50) num
  if (num .ne. z'40CE7D13' ) error stop 9

  read(2,50) num
  if (num .ne. z'40CE7D06' ) error stop 10

  read(2,60) num
  if (num .ne. z'C0CE7D13' ) error stop 11

  read(2,60) num
  if (num .ne. z'C0CE7D06' ) error stop 12

  close(2)

  !Test Read statement with rounding mode nearest
  open(UNIT=2,FILE='real4.dat')

  read(2,70) num
  if (num .ne. z'40CE7D13' ) error stop 13

  read(2,70) num
  if (num .ne. z'40CE7D07' ) error stop 14

  read(2,80) num
  if (num .ne. z'C0CE7D13' ) error stop 15

  read(2,80) num
  if (num .ne. z'C0CE7D07' ) error stop 16

  close(2)

  !Test Read statement with rounding mode compatible
  open(UNIT=2,FILE='real4.dat')

  read(2,90) num
  if (num .ne. z'40CE7D13' ) error stop 17

  read(2,90) num
  if (num .ne. z'40CE7D07' ) error stop 18

  read(2,100) num
  if (num .ne. z'C0CE7D13' ) error stop 19

  read(2,100) num
  if (num .ne. z'C0CE7D07' ) error stop 20

  close(2)

  !Test Read Statement with rounding mode processor_defined
  open(UNIT=2,FILE='real4.dat')

  read(2,110) num
  if (num .ne. z'40CE7D13' ) error stop 21

  read(2,110) num
  if (num .ne. z'40CE7D07' ) error stop 22

  read(2,120) num
  if (num .ne. z'C0CE7D13' ) error stop 23

  read(2,120) num
  if (num .ne. z'C0CE7D07' ) error stop 24

  close(2)

  !Test edit descriptors with run time encoding
  round_mode='(ru,f8.6)'
  open(UNIT=2,FILE='real4.dat')

  read(2,round_mode) num
  if (num .ne. z'40CE7D14') error stop 25

  read(2,round_mode) num
  if (num .ne. z'40CE7D07' ) error stop 26

  round_mode='(ru,f9.6)'
  read(2,round_mode) num
  if (num .ne. z'C0CE7D13' ) error stop 27

  read(2,round_mode) num
  if (num .ne. z'C0CE7D06' ) error stop 28

  close(2)

  round_mode='(rd,f8.6)'
  open(UNIT=2,FILE='real4.dat')

  read(2,round_mode) num
  if (num .ne. z'40CE7D13') error stop 29

  read(2,round_mode) num
  if (num .ne. z'40CE7D06' ) error stop 30

  round_mode='(rd,f9.6)'
  read(2,round_mode) num
  if (num .ne. z'C0CE7D14' ) error stop 31

  read(2,round_mode) num
  if (num .ne. z'C0CE7D07' ) error stop 32

  close(2)

  round_mode='(rz,f8.6)'
  open(UNIT=2,FILE='real4.dat')

  read(2,round_mode) num
  if (num .ne. z'40CE7D13') error stop 33

  read(2,round_mode) num
  if (num .ne. z'40CE7D06' ) error stop 34

  round_mode='(rz,f9.6)'
  read(2,round_mode) num
  if (num .ne. z'C0CE7D13' ) error stop 35

  read(2,round_mode) num
  if (num .ne. z'C0CE7D06' ) error stop 36

  close(2)

  round_mode='(rn,f8.6)'
  open(UNIT=2,FILE='real4.dat')

  read(2,round_mode) num
  if (num .ne. z'40CE7D13') error stop 37

  read(2,round_mode) num
  if (num .ne. z'40CE7D07' ) error stop 38

  round_mode='(rn,f9.6)'
  read(2,round_mode) num
  if (num .ne. z'C0CE7D13' ) error stop 39

  read(2,round_mode) num
  if (num .ne. z'C0CE7D07' ) error stop 40

  close(2)

  round_mode='(rc,f8.6)'
  open(UNIT=2,FILE='real4.dat')
  read(2,round_mode) num
  if (num .ne. z'40CE7D13') error stop 41

  read(2,round_mode) num
  if (num .ne. z'40CE7D07' ) error stop 42

  round_mode='(rc,f9.6)'
  read(2,round_mode) num
  if (num .ne. z'C0CE7D13' ) error stop 43

  read(2,round_mode) num
  if (num .ne. z'C0CE7D07' ) error stop 44

  close(2)

  round_mode='(rp,f8.6)'
  open(UNIT=2,FILE='real4.dat')

  read(2,round_mode) num
  if (num .ne. z'40CE7D13') error stop 45

  read(2,round_mode) num
  if (num .ne. z'40CE7D07' ) error stop 46

  round_mode='(rp,f9.6)'
  read(2,round_mode) num
  if (num .ne. z'C0CE7D13' ) error stop 47

  read(2,round_mode) num
  if (num .ne. z'C0CE7D07' ) error stop 48

  close(2)

end
