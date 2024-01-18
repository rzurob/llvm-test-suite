! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : June. 20, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common must be specified
!*                              : as common block in same scope unit.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
integer x
bind(c) /blk/
common /blk/ x
end module
program bcdasc36
use mod
implicit none
bind(c) /blk/
end program
