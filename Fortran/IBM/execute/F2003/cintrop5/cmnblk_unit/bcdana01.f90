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
!* DESCRIPTION                  : Test the binding label can not be
!*                              : specified for other global variables.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
integer x1, x2
common /blk1/ x1
common /blk2/ x2
bind(c, name="foo") /blk1/, /blk2/
end module
program bcdana01
end program
