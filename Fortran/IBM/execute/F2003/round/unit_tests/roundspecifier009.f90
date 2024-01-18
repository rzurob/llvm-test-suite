!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier009.f
! %VERifY:
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
!*  TEST CASE TITLE            : roundspecifier009
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
!*  DESCRIPTION                : functional testing of ROUND= specifiers
!*                               with specifers set at compile and run time with
!*                               external files
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num
  
  character(20) :: round_mode
  
 
  !Test Read statement with rounding mode up
  open(UNIT=2,FILE='real4.dat')
  
  read(unit=2,FMT='(f8.6)', ROUND='up') num 
  if (num .ne. z'40CE7D14') error stop 1
   
  read(unit=2,FMT='(f8.6)', ROUND='up') num
  if (num .ne. z'40CE7D07' ) error stop 2
  read(unit=2,FMT='(f9.6)', ROUND='up') num
  if (num .ne. z'C0CE7D13' ) error stop 3
  
  read(unit=2,FMT='(f9.6)', ROUND='up') num
  if (num .ne. z'C0CE7D06' ) error stop 4
  
  close(2)
 

  !Test Read statement with rounding mode down
  open(UNIT=2,FILE='real4.dat')
  
  read(unit=2,FMT='(f8.6)', ROUND='down') num
  if (num .ne. z'40CE7D13' ) error stop 5
   
  read(unit=2,FMT='(f8.6)', ROUND='down') num
  if (num .ne. z'40CE7D06' ) error stop 6
  
  read(unit=2,FMT='(f9.6)', ROUND='down') num
  if (num .ne. z'C0CE7D14' ) error stop 7
  
  read(unit=2,FMT='(f9.6)', ROUND='down') num
  if (num .ne. z'C0CE7D07' ) error stop 8
  
  close(2)

  !Test Read statement with roundming mode zero
  open(UNIT=2,FILE='real4.dat')
  
  read(unit=2,FMT='(f8.6)', ROUND='zero') num
  if (num .ne. z'40CE7D13' ) error stop 9
   
  read(unit=2,FMT='(f8.6)', ROUND='zero') num
  if (num .ne. z'40CE7D06' ) error stop 10
  
  read(unit=2,FMT='(f9.6)', ROUND='zero') num
  if (num .ne. z'C0CE7D13' ) error stop 11
  
  read(unit=2,FMT='(f9.6)', ROUND='zero') num
  if (num .ne. z'C0CE7D06' ) error stop 12
  
  close(2)

  !Test Read statement with rounding mode nearest
  open(UNIT=2,FILE='real4.dat')
  
  read(unit=2,FMT='(f8.6)', ROUND='nearest') num
  if (num .ne. z'40CE7D13' ) error stop 13
   
  read(unit=2,FMT='(f8.6)', ROUND='nearest') num
  if (num .ne. z'40CE7D07' ) error stop 14
  
  read(unit=2,FMT='(f9.6)', ROUND='nearest') num
  if (num .ne. z'C0CE7D13' ) error stop 15
  
  read(unit=2,FMT='(f9.6)', ROUND='nearest') num
  if (num .ne. z'C0CE7D07' ) error stop 16
  
  close(2)
  
  !Test Read statement with rounding mode compatible
  open(UNIT=2,FILE='real4.dat')
  
  read(unit=2,FMT='(f8.6)', ROUND='compatible') num
  if (num .ne. z'40CE7D13' ) error stop 17
   
  read(unit=2,FMT='(f8.6)', ROUND='compatible') num
  if (num .ne. z'40CE7D07' ) error stop 18
  
  read(unit=2,FMT='(f9.6)', ROUND='compatible') num
  if (num .ne. z'C0CE7D13' ) error stop 19
  
  read(unit=2,FMT='(f9.6)', ROUND='compatible') num
  if (num .ne. z'C0CE7D07' ) error stop 20
  
  close(2)
  
  !Test Read Statement with rounding mode processor_defined
  open(UNIT=2,FILE='real4.dat')
  
  read(unit=2,FMT='(f8.6)', ROUND='processor_defined') num
  if (num .ne. z'40CE7D13' ) error stop 21
   
  read(unit=2,FMT='(f8.6)', ROUND='processor_defined') num
  if (num .ne. z'40CE7D07' ) error stop 22
  
  read(unit=2,FMT='(f9.6)', ROUND='processor_defined') num
  if (num .ne. z'C0CE7D13' ) error stop 23
  
  read(unit=2,FMT='(f9.6)', ROUND='processor_defined') num
  if (num .ne. z'C0CE7D07' ) error stop 24
  
  close(2)
  
  round_mode='up'
  open(UNIT=2,FILE='real4.dat')
  
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num
  if (num .ne. z'40CE7D14' ) error stop 25
   
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num
  if (num .ne. z'40CE7D07' ) error stop 26
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D13' ) error stop 27
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D06' ) error stop 28
  
  close(2)
  
  round_mode='down'
  open(UNIT=2,FILE='real4.dat')
 
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num 
  if (num .ne. z'40CE7D13' ) error stop 29
   
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num
  if (num .ne. z'40CE7D06' ) error stop 30
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D14' ) error stop 31
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D07' ) error stop 32
  
  close(2)
  
  round_mode='zero'
  open(UNIT=2,FILE='real4.dat')
 
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num 
  if (num .ne. z'40CE7D13' ) error stop 33
   
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num
  if (num .ne. z'40CE7D06' ) error stop 34
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D13' ) error stop 35
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D06' ) error stop 36
  
  close(2)
  
  round_mode='nearest'
  open(UNIT=2,FILE='real4.dat')
  
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num
  if (num .ne. z'40CE7D13' ) error stop 37
   
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num
  if (num .ne. z'40CE7D07' ) error stop 38
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D13' ) error stop 39
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D07' ) error stop 40
  
  close(2)
  
  round_mode='compatible'
  open(UNIT=2,FILE='real4.dat')
  
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num
  if (num .ne. z'40CE7D13' ) error stop 41
   
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num
  if (num .ne. z'40CE7D07' ) error stop 42
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D13' ) error stop 43
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D07' ) error stop 44
  
  close(2)
  
  round_mode='processor_defined'
  open(UNIT=2,FILE='real4.dat')
  
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num
  if (num .ne. z'40CE7D13' ) error stop 45
   
  read(unit=2,FMT='(f8.6)', ROUND=round_mode) num
  if (num .ne. z'40CE7D07' ) error stop 46
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D13' ) error stop 47
  
  read(unit=2,FMT='(f9.6)', ROUND=round_mode) num
  if (num .ne. z'C0CE7D07' ) error stop 48
  
  close(2)
end  
