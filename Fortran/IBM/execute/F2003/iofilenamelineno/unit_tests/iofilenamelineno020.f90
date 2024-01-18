!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 1, 2020
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

  integer :: num1

  call setrteopts('errloc=no')

  open(unit=2,form='formatted', asynch='no', access='sequential' )

  read(unit=2, fmt='(i8)', round='up', decimal='point', pad='yes', asynch='yes') num1

end