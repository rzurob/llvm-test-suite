! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : July 28, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common blocks work with
!*                              : -qextchk
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdachk1
implicit none
integer x
real y, z
common /blk/ x, y, z
bind(c) :: /blk/
x = 1
y = 2.0
z = 3.0
call csub()
end program
