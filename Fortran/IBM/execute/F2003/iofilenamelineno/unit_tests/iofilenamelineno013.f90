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
integer :: unit_num=-2
integer :: i

call setrteopts('errloc=no')

open(unit=unit_num)

rewind(unit_num)

end

