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
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
integer x
common /blk/ x
bind(c) :: /blk/
end module
program bcuncm13
use mod
implicit none
x = 1
print *, x
call csub()
print *, x
end program