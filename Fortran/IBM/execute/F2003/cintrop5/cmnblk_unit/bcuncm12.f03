! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : May. 24, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test bind(c) common blocks in module
!*                              : work as global variables.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  07/19/04    KV    - corrected error of "character(1) x" trying to be
!*                      ineroperable with "char x[1]" (defect 285281)
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
  integer x
  real y
  character z(1)
  common /blk/ x, y, z
  bind(c, name="bar") :: /blk/
end module
program bcuncm12
  use mod
  x = 1
  y = 2.0
  z = 'F'
  print *, x, y, z
  call csub()
  print *, x, y, z
end program