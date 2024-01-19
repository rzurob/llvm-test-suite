! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Sept. 08, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common block work with
!*                              : -qattr
!234567890123456789012345678901234567890123456789012345678901234567890
program bcunatt1
implicit none
integer x
real y
common /blk/ x, y
bind(c) /blk/
x = 1
y = 2.0
end program
