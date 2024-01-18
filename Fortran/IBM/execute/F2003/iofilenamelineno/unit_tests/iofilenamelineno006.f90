!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 1, 2006
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

  call setrteopts("errloc=yes")

  format_spec='(f8)'

  open(unit=2,file='iofilenamelineno006.dat')

  read(unit=2,fmt=format_spec, round='nearest', decimal='point', blank='null', pad='yes') num
end