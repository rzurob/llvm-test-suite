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
real y
character(1) z
common /blk/ x, y, z
bind(c) :: /blk/
end module
program bcuncm11
use mod
implicit none
x = 1
y = 2.0
z = 'F'
print *, x, y, z
call csub()
print *, x, y, z
end program
