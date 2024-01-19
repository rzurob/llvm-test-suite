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
program bcdasc25
implicit none
integer x
bind(c) /blk/
common /blk/ x
contains
subroutine sub
implicit none
bind(c) /blk/
end subroutine
end program
