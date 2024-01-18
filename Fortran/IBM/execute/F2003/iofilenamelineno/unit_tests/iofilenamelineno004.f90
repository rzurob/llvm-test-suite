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

  character(5) :: status_spec

  call setrteopts("errloc=yes")

  status_spec="old"

  open(unit=2, file="iofilenamelineno004.dat", status=status_spec, round='up')

end