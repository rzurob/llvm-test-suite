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
!*                              : should not be specified as pointer.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
integer, pointer :: x
bind(c) /blk/
common /blk/ x
end module

program bcdapt1a
end program
