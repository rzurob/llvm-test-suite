!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 1, 2007
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

  real :: num
  integer :: unit_num

  call setrteopts("errloc=yes")

  unit_num=2

  open(unit=unit_num, file="iofilenamelineno007.dat")

  unit_num=3

  open(unit=unit_num, file="iofilenamelineno007.dat", round='nearest', decimal='point')
end