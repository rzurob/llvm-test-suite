!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iofilenamelineno015.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 1, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Display file name and line no in I/O failures
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :Test that file name and line no are displayed for
!*                              I/O failures at runtime
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  character(20) :: format_spec
  real :: num

  call setrteopts('errloc=no')

  format_spec='f8.6)'
  num=5.0

  open(unit=2,file='iofilenamelineno015.dat')

  write(unit=2,fmt=format_spec, round='up', decimal='point', sign='processor_defined') num
end