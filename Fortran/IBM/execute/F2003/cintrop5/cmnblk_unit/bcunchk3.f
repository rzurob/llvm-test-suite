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
program bcunchk3
implicit none
integer x
common /blk/ x
bind(c) :: /blk/
x = 1
print *, x
call csub()
print *, x
end program
