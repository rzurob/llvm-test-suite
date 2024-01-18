!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier026.f
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
!*  TEST CASE TITLE            : roundspecifier026
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
!*  DESCRIPTION                : functional testing of ROUND= specifier in 
!*                               READ, WRITE statements with complex(4)
!*                               
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  complex(4) :: write_num1,write_num2,write_num3, write_num4
  complex(4) :: read_num1, read_num2, read_num3, read_num4  

  character(25) :: round_mode
  

  write_num1=(6.452768,6.452768)
  write_num2=(6.452762,6.452762)
  write_num3=(-6.452768,-6.452768)
  write_num4=(-6.452762,-6.452762)
  
  open(2, file='roundspecifier026.out', ROUND='up')
  open(3, file='complex4.dat', ROUND='down')

  write(UNIT=2,FMT='(2f9.5)', ROUND='up') write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND='up') read_num1
  if (real(read_num1) .ne. z'40CE7D14' .or. aimag(read_num1) .ne. z'40CE7D14') error stop 1
   
  write(UNIT=2,FMT='(2f9.5)', ROUND='up') write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND='up') read_num2
  if (real(read_num2) .ne. z'40CE7D07' .or. aimag(read_num2) .ne. z'40CE7D07') error stop 2
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='up') write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND='up') read_num3
  if (real(read_num3) .ne. z'C0CE7D13'.or. aimag(read_num3) .ne. z'C0CE7D13') error stop 3
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='up') write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND='up') read_num4
  if (real(read_num4) .ne. z'C0CE7D06'.or. aimag(read_num4) .ne. z'C0CE7D13') error stop 4
  
  rewind 3
  
  write(UNIT=2,FMT='(2f9.5)', ROUND='down') write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND='down') read_num1
  if (real(read_num1) .ne. z'40CE7D13'.or. aimag(read_num1) .ne. z'40CE7D13') error stop 5
   
  write(UNIT=2,FMT='(2f9.5)', ROUND='down') write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND='down') read_num2
  if (real(read_num2) .ne. z'40CE7D06'.or. aimag(read_num2) .ne. z'40CE7D06') error stop 6
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='down') write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND='down') read_num3
  if (real(read_num3) .ne. z'C0CE7D14'.or. aimag(read_num3) .ne. z'C0CE7D14') error stop 7
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='down') write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND='down') read_num4
  if (real(read_num4) .ne. z'C0CE7D07'.or. aimag(read_num4) .ne. z'C0CE7D14') error stop 8
  
  rewind 3

  write(UNIT=2,FMT='(2f9.5)', ROUND='zero') write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND='zero') read_num1
  if (real(read_num1) .ne. z'40CE7D13' .or. aimag(read_num1) .ne. z'40CE7D13') error stop 9
   
  write(UNIT=2,FMT='(2f9.5)', ROUND='zero') write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND='zero') read_num2
  if (real(read_num2) .ne. z'40CE7D06' .or. aimag(read_num2) .ne. z'40CE7D06') error stop 10
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='zero') write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND='zero') read_num3
  if (real(read_num3) .ne. z'C0CE7D13' .or. aimag(read_num3) .ne. z'C0CE7D13') error stop 11
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='zero') write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND='zero') read_num4
  if (real(read_num4) .ne. z'C0CE7D06' .or. aimag(read_num4) .ne. z'C0CE7D13') error stop 12
  
  rewind 3
   
  write(UNIT=2,FMT='(2f9.5)', ROUND='nearest') write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND='nearest') read_num1
  if (real(read_num1) .ne. z'40CE7D13' .or. aimag(read_num1) .ne. z'40CE7D13') error stop 13
   
  write(UNIT=2,FMT='(2f9.5)', ROUND='nearest') write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND='nearest') read_num2
  if (real(read_num2) .ne. z'40CE7D07' .or. aimag(read_num2) .ne. z'40CE7D07') error stop 14
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='nearest') write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND='nearest') read_num3
  if (real(read_num3) .ne. z'C0CE7D13' .or. aimag(read_num3) .ne. z'C0CE7D13') error stop 15
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='nearest') write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND='nearest') read_num4
  if (real(read_num4) .ne. z'C0CE7D07' .or. aimag(read_num4) .ne. z'C0CE7D13') error stop 16
  
  rewind 3
  
  write(UNIT=2,FMT='(2f9.5)', ROUND='compatible') write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND='compatible') read_num1
  if (real(read_num1) .ne. z'40CE7D13' .or. aimag(read_num1) .ne. z'40CE7D13') error stop 17
   
  write(UNIT=2,FMT='(2f9.5)', ROUND='compatible') write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND='compatible') read_num2
  if (real(read_num2) .ne. z'40CE7D07' .or. aimag(read_num2) .ne. z'40CE7D07') error stop 18
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='compatible') write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND='compatible') read_num3
  if (real(read_num3) .ne. z'C0CE7D13' .or. aimag(read_num3) .ne. z'C0CE7D13') error stop 19
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='compatible') write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND='compatible') read_num4
  if (real(read_num4) .ne. z'C0CE7D07' .or. aimag(read_num4) .ne. z'C0CE7D13') error stop 20
  
  rewind 3
  
  write(UNIT=2,FMT='(2f9.5)', ROUND='processor_defined') write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND='processor_defined') read_num1
  if (real(read_num1) .ne. z'40CE7D13' .or. aimag(read_num1) .ne. z'40CE7D13') error stop 21
   
  write(UNIT=2,FMT='(2f9.5)', ROUND='processor_defined') write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND='processor_defined') read_num2
  if (real(read_num2) .ne. z'40CE7D07' .or. aimag(read_num2) .ne. z'40CE7D07') error stop 22
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='processor_defined') write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND='processor_defined') read_num3
  if (real(read_num3) .ne. z'C0CE7D13' .or. aimag(read_num3) .ne. z'C0CE7D13') error stop 23
  
  write(UNIT=2,FMT='(2f10.5)', ROUND='processor_defined') write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND='processor_defined') read_num4
  if (real(read_num4) .ne. z'C0CE7D07' .or. aimag(read_num4) .ne. z'C0CE7D13') error stop 24
  
  rewind 3
  
  round_mode='up'
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num1
  if (real(read_num1) .ne. z'40CE7D14' .or. aimag(read_num1) .ne. z'40CE7D14') error stop 25
   
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num2
  if (real(read_num2) .ne. z'40CE7D07' .or. aimag(read_num2) .ne. z'40CE7D07') error stop 26
  
  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num3
  if (real(read_num3) .ne. z'C0CE7D13' .or. aimag(read_num3) .ne. z'C0CE7D13') error stop 27
  
  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num4
  if (real(read_num4) .ne. z'C0CE7D06' .or. aimag(read_num4) .ne. z'C0CE7D13') error stop 28
  
  rewind 3
  
  round_mode='down'
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num1
  if (real(read_num1) .ne. z'40CE7D13' .or. aimag(read_num1) .ne. z'40CE7D13') error stop 29
   
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num2
  if (real(read_num2) .ne. z'40CE7D06' .or. aimag(read_num2) .ne. z'40CE7D06') error stop 30
  
  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num3
  if (real(read_num3) .ne. z'C0CE7D14' .or. aimag(read_num3) .ne. z'C0CE7D14') error stop 31
  
  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num4
  if (real(read_num4) .ne. z'C0CE7D07' .or. aimag(read_num4) .ne. z'C0CE7D14') error stop 32
  
  rewind 3
  
  round_mode='zero'
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num1
  if (real(read_num1) .ne. z'40CE7D13' .or. aimag(read_num1) .ne. z'40CE7D13') error stop 33
   
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num2
  if (real(read_num2) .ne. z'40CE7D06' .or. aimag(read_num2) .ne. z'40CE7D06') error stop 34

  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num3
  if (real(read_num3) .ne. z'C0CE7D13' .or. aimag(read_num3) .ne. z'C0CE7D13') error stop 35
  
  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num4
  if (real(read_num4) .ne. z'C0CE7D06' .or. aimag(read_num4) .ne. z'C0CE7D13') error stop 36
  
  rewind 3
  
  round_mode='nearest'
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num1
  if (real(read_num1) .ne. z'40CE7D13' .or. aimag(read_num1) .ne. z'40CE7D13') error stop 37
   
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num2
  if (real(read_num2) .ne. z'40CE7D07' .or. aimag(read_num2) .ne. z'40CE7D07') error stop 38

  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num3
  if (real(read_num3) .ne. z'C0CE7D13' .or. aimag(read_num3) .ne. z'C0CE7D13') error stop 39
  
  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num4
  if (real(read_num4) .ne. z'C0CE7D07' .or. aimag(read_num4) .ne. z'C0CE7D13') error stop 40
  
  rewind 3
  
  round_mode='compatible'
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num1
  if (real(read_num1) .ne. z'40CE7D13' .or. aimag(read_num1) .ne. z'40CE7D13') error stop 41
   
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num2
  if (real(read_num2) .ne. z'40CE7D07' .or. aimag(read_num2) .ne. z'40CE7D07') error stop 42
  
  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num3
  if (real(read_num3) .ne. z'C0CE7D13' .or. aimag(read_num3) .ne. z'C0CE7D13') error stop 43
  
  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num4
  if (real(read_num4) .ne. z'C0CE7D07' .or. aimag(read_num4) .ne. z'C0CE7D13') error stop 44
  
  rewind 3
  
  round_mode='processor_defined'
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num1
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num1
  if (real(read_num1) .ne. z'40CE7D13' .or. aimag(read_num1) .ne. z'40CE7D13') error stop 45
   
  write(UNIT=2,FMT='(2f9.5)', ROUND=round_mode) write_num2
  read(UNIT=3,FMT='(2f9.6)', ROUND=round_mode) read_num2
  if (real(read_num2) .ne. z'40CE7D07' .or. aimag(read_num2) .ne. z'40CE7D07') error stop 46
  
  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num3
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num3
  if (real(read_num3) .ne. z'C0CE7D13' .or. aimag(read_num3) .ne. z'C0CE7D13') error stop 46
  
  write(UNIT=2,FMT='(2f10.5)', ROUND=round_mode) write_num4
  read(UNIT=3,FMT='(2f10.6)', ROUND=round_mode) read_num4
  if (real(read_num4) .ne. z'C0CE7D07' .or. aimag(read_num4) .ne. z'C0CE7D13') error stop 48
 end 
