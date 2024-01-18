!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier034.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : roundspecifier034
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : Dec. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : functional testing of round edit descriptor in 
!*                               READ, WRITE statements with
!*                               es edit descriptor 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  real(4) :: num1, num2, num3, num4
  real(8) :: num5, num6, num7, num8
  real(16) :: num9, num10, num11, num12

  character(20) :: round_mode

  namelist /namelist_real/ num1, num2, num3, num4, num5, num6, num7, num8, num9, num10, num11, num12

  open(unit=2, file='namelist_real.dat', round='nearest')
  
  round_mode='up'
  read(2,nml=namelist_real, round=round_mode)

  if (num1 .ne. z'40CE7D14') error stop 1
  if (num2 .ne. z'40CE7D07') error stop 2
  if (num3 .ne. z'C0CE7D13') error stop 3
  if (num4 .ne. z'C0CE7D06') error stop 4
  if (num5 .ne. z'4019CFA84384E869') error stop 5
  if (num6 .ne. z'4019CFA84384E826') error stop 6
  if (num7 .ne. z'C019CFA84384E868') error stop 7
  if (num8 .ne. z'C019CFA84384E825') error stop 8
  if (num9 .ne. z'4019CFA84384E97EBCC9A114DE4CF5AB') error stop 9
  if (num10 .ne. z'4019CFA84384E97EBCC9A114DE4D0D70') error stop 10
  if (num11 .ne. z'C019CFA84384E97E3CC9A114DE4CF5AB') error stop 11
  if (num12 .ne. z'C019CFA84384E97E3CC9A114DE4D0D70') error stop 12
  
  rewind 2
  
  round_mode='down'
  read(2,nml=namelist_real, round=round_mode)

  if (num1 .ne. z'40CE7D13') error stop 13
  if (num2 .ne. z'40CE7D06') error stop 14
  if (num3 .ne. z'C0CE7D14') error stop 15
  if (num4 .ne. z'C0CE7D07') error stop 16
  if (num5 .ne. z'4019CFA84384E868') error stop 17
  if (num6 .ne. z'4019CFA84384E825') error stop 18
  if (num7 .ne. z'C019CFA84384E869') error stop 19
  if (num8 .ne. z'C019CFA84384E826') error stop 20
  if (num9 .ne. z'4019CFA84384E97D3CA97BAC86CC2950') error stop 21
  if (num10 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D') error stop 22
  if (num11 .ne. z'C019CFA84384E97DBCA97BAC86CC2950') error stop 23
  if (num12 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D') error stop 24
  
  rewind 2
  
  round_mode='zero'
  read(2,nml=namelist_real, round=round_mode)

  if (num1 .ne. z'40CE7D13') error stop 25
  if (num2 .ne. z'40CE7D06') error stop 26
  if (num3 .ne. z'C0CE7D13') error stop 27
  if (num4 .ne. z'C0CE7D06') error stop 28
  if (num5 .ne. z'4019CFA84384E868') error stop 29
  if (num6 .ne. z'4019CFA84384E825') error stop 30
  if (num7 .ne. z'C019CFA84384E868') error stop 31
  if (num8 .ne. z'C019CFA84384E825') error stop 32
  if (num9 .ne. z'4019CFA84384E97D3CA97BAC86CC2950') error stop 33
  if (num10 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3D') error stop 34
  if (num11 .ne. z'C019CFA84384E97DBCA97BAC86CC2950') error stop 35
  if (num12 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3D') error stop 36
  
  rewind 2
  
  round_mode='nearest'
  read(2,nml=namelist_real, round=round_mode)

  if (num1 .ne. z'40CE7D13') error stop 37
  if (num2 .ne. z'40CE7D07') error stop 38
  if (num3 .ne. z'C0CE7D13') error stop 39
  if (num4 .ne. z'C0CE7D07') error stop 40
  if (num5 .ne. z'4019CFA84384E869') error stop 41
  if (num6 .ne. z'4019CFA84384E825') error stop 42
  if (num7 .ne. z'C019CFA84384E869') error stop 43
  if (num8 .ne. z'C019CFA84384E825') error stop 44
  if (num9 .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 45
  if (num10 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 46
  if (num11 .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 47
  if (num12 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 48
  
  rewind 2
  
  round_mode='compatible'
  read(2,nml=namelist_real, round=round_mode)

  if (num1 .ne. z'40CE7D13') error stop 49
  if (num2 .ne. z'40CE7D07') error stop 50
  if (num3 .ne. z'C0CE7D13') error stop 51
  if (num4 .ne. z'C0CE7D07') error stop 52
  if (num5 .ne. z'4019CFA84384E869') error stop 53
  if (num6 .ne. z'4019CFA84384E825') error stop 54
  if (num7 .ne. z'C019CFA84384E869') error stop 55
  if (num8 .ne. z'C019CFA84384E825') error stop 56
  if (num9 .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 57
  if (num10 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 58
  if (num11 .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 59
  if (num12 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 60
  
  rewind 2
  
  round_mode='processor_defined'
  read(2,nml=namelist_real, round=round_mode)

  if (num1 .ne. z'40CE7D13') error stop 61
  if (num2 .ne. z'40CE7D07') error stop 62
  if (num3 .ne. z'C0CE7D13') error stop 63
  if (num4 .ne. z'C0CE7D07') error stop 64
  if (num5 .ne. z'4019CFA84384E869') error stop 65
  if (num6 .ne. z'4019CFA84384E825') error stop 66
  if (num7 .ne. z'C019CFA84384E869') error stop 67
  if (num8 .ne. z'C019CFA84384E825') error stop 68
  if (num9 .ne. z'4019CFA84384E97D3CA97BAC86CC2951') error stop 69
  if (num10 .ne. z'4019CFA84384E97D3CA97BAC86CBCA3E') error stop 70
  if (num11 .ne. z'C019CFA84384E97DBCA97BAC86CC2951') error stop 71
  if (num12 .ne. z'C019CFA84384E97DBCA97BAC86CBCA3E') error stop 72
  
end
