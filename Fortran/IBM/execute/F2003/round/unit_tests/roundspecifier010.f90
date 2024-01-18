!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier010.f
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
!*  TEST CASE TITLE            : roundspecifier010
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
!*  DESCRIPTION                : functional testing of ROUND= specifier in WRITE statements
!*                               with specifers set at compile and runtime with
!*                               external files 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  
  real :: num1=6.452768
  real :: num2=6.452762
  real :: num3=-6.452768
  real :: num4=-6.452762
  
  character(20) :: round_mode
  
  open(unit=2, file='roundspecifier010.out')
  
  !Test with specifiers encoded at compile time
  !Test WRITE statement with rounding mode up
  write(UNIT=2,FMT='(f8.5)', ROUND='up') num1
  write(UNIT=2,FMT='(f8.5)', ROUND='up') num2
  write(UNIT=2,FMT='(f9.5)', ROUND='up') num3
  write(UNIT=2,FMT='(f9.5)', ROUND='up') num4
  
  !Test WRITE statement with rounding mode down
  write(UNIT=2,FMT='(f8.5)', ROUND='down') num1
  write(UNIT=2,FMT='(f8.5)', ROUND='down') num2
  write(UNIT=2,FMT='(f9.5)', ROUND='down') num3
  write(UNIT=2,FMT='(f9.5)', ROUND='down') num4
  
  !Test WRITE statement with rounding mode zero
  write(UNIT=2,FMT='(f8.5)', ROUND='zero') num1
  write(UNIT=2,FMT='(f8.5)', ROUND='zero') num2
  write(UNIT=2,FMT='(f9.5)', ROUND='zero') num3
  write(UNIT=2,FMT='(f9.5)', ROUND='zero') num4
  
  !Test WRITE statement with rounding mode nearest
  write(UNIT=2,FMT='(f8.5)', ROUND='nearest') num1
  write(UNIT=2,FMT='(f8.5)', ROUND='nearest') num2
  write(UNIT=2,FMT='(f9.5)', ROUND='nearest') num3
  write(UNIT=2,FMT='(f9.5)', ROUND='nearest') num4
  
  !Test WRITE statement with rounding mode compatible
  write(UNIT=2,FMT='(f8.5)', ROUND='compatible') num1
  write(UNIT=2,FMT='(f8.5)', ROUND='compatible') num2
  write(UNIT=2,FMT='(f9.5)', ROUND='compatible') num3
  write(UNIT=2,FMT='(f9.5)', ROUND='compatible') num4
  
  !Test WRITE statement with rounding mode processor_defined
  write(UNIT=2,FMT='(f8.5)', ROUND='processor_defined') num1
  write(UNIT=2,FMT='(f8.5)', ROUND='processor_defined') num2
  write(UNIT=2,FMT='(f9.5)', ROUND='processor_defined') num3
  write(UNIT=2,FMT='(f9.5)', ROUND='processor_defined') num4
  
  
  !Test with specifiers encoded at runtime
  round_mode='up'
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num1
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num2
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num3
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num4
  
  round_mode='down'
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num1
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num2
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num3
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num4
  
  round_mode='zero'
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num1
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num2
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num3
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num4
  
  round_mode='nearest'
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num1
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num2
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num3
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num4
  
  round_mode='compatible'
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num1
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num2
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num3
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num4
  
  round_mode='processor_defined'
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num1
  write(UNIT=2,FMT='(f8.5)', ROUND=round_mode) num2
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num3
  write(UNIT=2,FMT='(f9.5)', ROUND=round_mode) num4

end
