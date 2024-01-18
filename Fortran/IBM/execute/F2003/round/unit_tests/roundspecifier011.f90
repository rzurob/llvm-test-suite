!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier011.f
! %VERIFY:
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
!*  DESCRIPTION                : functional testing of ROUND= specifier in PRINT statement
!*                               with specifers set at compile and run time with
!*                               external files
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num1=6.452768
  real :: num2=6.452762
  real :: num3=-6.452768
  real :: num4=-6.452762

  character(20) :: round_mode

  10 format(ru,f8.5)
  20 format(ru,f9.5)
  30 format(rd,f8.5)
  40 format(rd,f9.5)
  50 format(rz,f8.5)
  60 format(rz,f9.5)
  70 format(rn,f8.5)
  80 format(rn,f9.5)
  90 format(rc,f8.5)
  100 format(rc,f9.5)
  110 format(rp,f8.5)
  120 format(rp,f9.5)

  !Test print statemet with round edit descriptors known at compile time
  print 10, num1
  print 10, num2
  print 20, num3
  print 20, num4

  print 30, num1
  print 30, num2
  print 40, num3
  print 40, num4

  print 50, num1
  print 50, num2
  print 60, num3
  print 60, num4

  print 70, num1
  print 70, num2
  print 80, num3
  print 80, num4

  print 90, num1
  print 90, num2
  print 100, num3
  print 100, num4

  print 110, num1
  print 110, num2
  print 120, num3
  print 120, num4

  !Test print statement with round edit descriptors known at run time
  round_mode='(ru,f8.5)'
  print round_mode, num1
  print round_mode, num2
  round_mode='(ru,f9.5)'
  print round_mode, num3
  print round_mode, num4

  round_mode='(rd,f8.5)'
  print round_mode, num1
  print round_mode, num2
  round_mode='(rd,f9.5)'
  print round_mode, num3
  print round_mode, num4

  round_mode='(rz,f8.5)'
  print round_mode, num1
  print round_mode, num2
  round_mode='(rz,f9.5)'
  print round_mode, num3
  print round_mode, num4

  round_mode='(rn,f8.5)'
  print round_mode, num1
  print round_mode, num2
  round_mode='(rn,f9.5)'
  print round_mode, num3
  print round_mode, num4

  round_mode='(rc,f8.5)'
  print round_mode, num1
  print round_mode, num2
  round_mode='(rc,f9.5)'
  print round_mode, num3
  print round_mode, num4

  round_mode='(rp,f8.5)'
  print round_mode, num1
  print round_mode, num2
  round_mode='(rp,f9.5)'
  print round_mode, num3
  print round_mode, num4

end
