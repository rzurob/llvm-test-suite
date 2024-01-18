!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier014.f
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
!*  TEST CASE TITLE            : roundspecifier014
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
!*                               with round edit descriptors set at compile and runtime with
!*                               external files 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  
  real :: num1=6.452768
  real :: num2=6.452762
  real :: num3=-6.452768
  real :: num4=-6.452762
  
  character(20) :: round_mode
  
  open(2, file='roundspecifier014.out')
  
  !Test edit descriptors encoded at compile time
  !Test WRITE statement with rounding mode up
  write(2,'(ru,f8.5)') num1
  write(2,'(ru,f8.5)') num2
  write(2,'(ru,f9.5)') num3
  write(2,'(ru,f9.5)') num4
  
  !Test WRITE statement with rounding mode down
  write(2,'(rd,f8.5)') num1
  write(2,'(rd,f8.5)') num2
  write(2,'(rd,f9.5)') num3
  write(2,'(rd,f9.5)') num4
  
  !Test WRITE statement with rounding mode zero
  write(2,'(rz,f8.5)') num1
  write(2,'(rz,f8.5)') num2
  write(2,'(rz,f9.5)') num3
  write(2,'(rz,f9.5)') num4
  
  !Test WRITE statement with rounding mode nearest
  write(2,'(rn,f8.5)') num1
  write(2,'(rn,f8.5)') num2
  write(2,'(rn,f9.5)') num3
  write(2,'(rn,f9.5)') num4
  
  !Test WRITE statement with rounding mode compatible
  write(2,'(rc,f8.5)') num1
  write(2,'(rc,f8.5)') num2
  write(2,'(rc,f9.5)') num3
  write(2,'(rc,f9.5)') num4
  
  !Test WRITE statement with rounding mode processor_defined
  write(2,'(rp,f8.5)') num1
  write(2,'(rp,f8.5)') num2
  write(2,'(rp,f9.5)') num3
  write(2,'(rp,f9.5)') num4
  
  
  !Test with specifiers encoded at runtime
  round_mode='(ru,f8.5)'
  write(2,round_mode) num1
  write(2,round_mode) num2
  round_mode='(ru,f9.5)'
  write(2,round_mode) num3
  write(2,round_mode) num4
  
  round_mode='(rd,f8.5)'
  write(2,round_mode) num1
  write(2,round_mode) num2
  round_mode='(rd,f9.5)'
  write(2,round_mode) num3
  write(2,round_mode) num4
  
  round_mode='(rz,f8.5)'
  write(2,round_mode) num1
  write(2,round_mode) num2
  round_mode='(rz,f9.5)'
  write(2,round_mode) num3
  write(2,round_mode) num4

  round_mode='(rn,f8.5)'
  write(2,round_mode) num1
  write(2,round_mode) num2
  round_mode='(rn,f9.5)'
  write(2,round_mode) num3
  write(2,round_mode) num4
  
  round_mode='(rc,f8.5)'
  write(2,round_mode) num1
  write(2,round_mode) num2
  round_mode='(rc,f9.5)'
  write(2,round_mode) num3
  write(2,round_mode) num4

  round_mode='(rp,f8.5)'
  write(2,round_mode) num1
  write(2,round_mode) num2
  round_mode='(rp,f9.5)'
  write(2,round_mode) num3
  write(2,round_mode) num4

end
