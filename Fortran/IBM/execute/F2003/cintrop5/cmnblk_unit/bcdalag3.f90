! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : July. 28, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!* DESCRIPTION                  : Language level checking for BIND(C)
!*                              : attribute.
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdalag3
implicit none
integer x
bind(c) :: /blk/
common /blk/ x
end program
