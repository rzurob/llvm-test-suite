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
!*  DESCRIPTION                : functional testing of ROUND= specifier in WRITE statements
!*                               with specifers set at compile and runtime with
!*                               external files
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num1=6.45279031275468
  real :: num2=6.45279031275462
  real :: num3=-6.45279031275468
  real :: num4=-6.45279031275462

  character(20) :: round_mode

  open(2, file='roundspecifier038.out')

  !Test with specifiers encoded at compile time
  !Test WRITE statement with rounding mode up
  write(2,*, ROUND='up') num1
  write(2,*, ROUND='up') num2
  write(2,*, ROUND='up') num3
  write(2,*, ROUND='up') num4

  !Test WRITE statement with rounding mode down
  write(2,*, ROUND='down') num1
  write(2,*, ROUND='down') num2
  write(2,*, ROUND='down') num3
  write(2,*, ROUND='down') num4

  !Test WRITE statement with rounding mode zero
  write(2,*, ROUND='zero') num1
  write(2,*, ROUND='zero') num2
  write(2,*, ROUND='zero') num3
  write(2,*, ROUND='zero') num4

  !Test WRITE statement with rounding mode nearest
  write(2,*, ROUND='nearest') num1
  write(2,*, ROUND='nearest') num2
  write(2,*, ROUND='nearest') num3
  write(2,*, ROUND='nearest') num4

  !Test WRITE statement with rounding mode compatible
  write(2,*, ROUND='compatible') num1
  write(2,*, ROUND='compatible') num2
  write(2,*, ROUND='compatible') num3
  write(2,*, ROUND='compatible') num4

  !Test WRITE statement with rounding mode processor_defined
  write(2,*, ROUND='processor_defined') num1
  write(2,*, ROUND='processor_defined') num2
  write(2,*, ROUND='processor_defined') num3
  write(2,*, ROUND='processor_defined') num4


  !Test with specifiers encoded at runtime
  round_mode='up'
  write(2,*, ROUND=round_mode) num1
  write(2,*, ROUND=round_mode) num2
  write(2,*, ROUND=round_mode) num3
  write(2,*, ROUND=round_mode) num4

  round_mode='down'
  write(2,*, ROUND=round_mode) num1
  write(2,*, ROUND=round_mode) num2
  write(2,*, ROUND=round_mode) num3
  write(2,*, ROUND=round_mode) num4

  round_mode='zero'
  write(2,*, ROUND=round_mode) num1
  write(2,*, ROUND=round_mode) num2
  write(2,*, ROUND=round_mode) num3
  write(2,*, ROUND=round_mode) num4

  round_mode='nearest'
  write(2,*, ROUND=round_mode) num1
  write(2,*, ROUND=round_mode) num2
  write(2,*, ROUND=round_mode) num3
  write(2,*, ROUND=round_mode) num4

  round_mode='compatible'
  write(2,*, ROUND=round_mode) num1
  write(2,*, ROUND=round_mode) num2
  write(2,*, ROUND=round_mode) num3
  write(2,*, ROUND=round_mode) num4

  round_mode='processor_defined'
  write(2,*, ROUND=round_mode) num1
  write(2,*, ROUND=round_mode) num2
  write(2,*, ROUND=round_mode) num3
  write(2,*, ROUND=round_mode) num4

end