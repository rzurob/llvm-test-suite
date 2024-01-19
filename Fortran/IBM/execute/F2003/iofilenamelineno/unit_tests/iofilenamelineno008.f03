!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 1, 2008
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

  complex :: num

  call setrteopts("errloc=yes")

  open(unit=2, file='iofilenamelineno008.dat')

  read(2,*,round='nearest', decimal='point', blank='null', pad='yes') num

end