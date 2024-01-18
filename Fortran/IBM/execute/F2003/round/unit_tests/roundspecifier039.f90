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
!*  DESCRIPTION                : functional testing of round edit descriptor in
!*                               READ, WRITE statements with
!*                               es edit descriptor
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  real(16) :: num

  character(20) :: round_mode

  open(2,FILE='real16.dat', round='nearest')

  !Test Read statement with rounding mode up

  read(2,*, ROUND='up') num
  if (num .ne. z'4019CFA84384E97EBCC9A114DE4CF5AB') error stop 1

  read(2,*, ROUND='up') num
  if (num .ne. z'4019CFA84384E97EBCC9A114DE4D0D70' ) error stop 2

  read(2,*, ROUND='up') num
  if (num .ne. z'C019CFA84384E97E3CC9A114DE4CF5AB' ) error stop 3

  read(2,*, ROUND='up') num
  if (num .ne. z'C019CFA84384E97E3CC9A114DE4D0D70' ) error stop 4

  rewind 2

  !Test Read statement with rounding mode down

  read(2,*, ROUND='down') num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CC2950' ) error stop 5

  read(2,*, ROUND='down') num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D' ) error stop 6

  read(2,*, ROUND='down') num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CC2950' ) error stop 7

  read(2,*, ROUND='down') num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D' ) error stop 8

  rewind 2

  !Test Read statement with roundming mode zero

  read(2,*, ROUND='zero') num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CC2950' ) error stop 9

  read(2,*, ROUND='zero') num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D' ) error stop 10

  read(2,*, ROUND='zero') num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CC2950' ) error stop 11

  read(2,*, ROUND='zero') num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D' ) error stop 12

  rewind 2

  !Test Read statement with rounding mode nearest

  read(2,*, ROUND='nearest') num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CC2951' ) error stop 13

  read(2,*, ROUND='nearest') num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' ) error stop 14

  read(2,*, ROUND='nearest') num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CC2951' ) error stop 15

  read(2,*, ROUND='nearest') num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' ) error stop 16

  rewind 2

  !Test Read statement with rounding mode compatible

  read(2,*, ROUND='compatible') num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CC2951' ) error stop 17

  read(2,*, ROUND='compatible') num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' ) error stop 18

  read(2,*, ROUND='compatible') num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CC2951' ) error stop 19

  read(2,*, ROUND='compatible') num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' ) error stop 20

  rewind 2

  !Test Read Statement with rounding mode processor_defined

  read(2,*, ROUND='processor_defined') num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CC2951' ) error stop 21

  read(2,*, ROUND='processor_defined') num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' ) error stop 22

  read(2,*, ROUND='processor_defined') num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CC2951' ) error stop 23

  read(2,*, ROUND='processor_defined') num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' ) error stop 24

  rewind 2

  round_mode='up'

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97EBCC9A114DE4CF5AB' ) error stop 25

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97EBCC9A114DE4D0D70' ) error stop 26

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97E3CC9A114DE4CF5AB' ) error stop 27

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97E3CC9A114DE4D0D70' ) error stop 28

  rewind 2

  round_mode='down'

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CC2950' ) error stop 29

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D' ) error stop 30

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CC2950' ) error stop 31

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D' ) error stop 32

  rewind 2

  round_mode='zero'

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CC2950' ) error stop 33

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D' ) error stop 34

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CC2950' ) error stop 35

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D' ) error stop 36

  rewind 2

  round_mode='nearest'

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CC2951' ) error stop 37

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' ) error stop 38

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CC2951' ) error stop 39

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' ) error stop 40

  rewind 2

  round_mode='compatible'

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CC2951' ) error stop 41

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' ) error stop 42

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CC2951' ) error stop 43

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' ) error stop 44

  rewind 2

  round_mode='processor_defined'

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CC2951' ) error stop 45

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E' ) error stop 46

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CC2951' ) error stop 47

  read(2,*, ROUND=round_mode) num
  if (num .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E' ) error stop 48

end
