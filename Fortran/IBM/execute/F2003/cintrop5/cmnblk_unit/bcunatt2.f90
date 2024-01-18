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
module mod
implicit none
integer x
real y
common /blk/ x, y
bind(c, name="foo") /blk/
end module
program bcunatt2
end program
