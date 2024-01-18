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
!* DESCRIPTION                  : Test bind(c) common block members
!*                              : should not be specified as non
!*                              : -interoperable variables
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdaty5a
implicit none
bind(c) /blk/
common /blk/ y
character(10) :: y
end program
