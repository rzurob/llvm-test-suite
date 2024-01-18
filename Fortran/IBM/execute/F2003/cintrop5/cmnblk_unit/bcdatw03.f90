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
!* DESCRIPTION                  : Test bind(c) common block should
!*                              : not be defined twice.
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdatw03
implicit none
integer x
common /blk/ x
bind(c) /blk/
bind(c) /blk/
end program
