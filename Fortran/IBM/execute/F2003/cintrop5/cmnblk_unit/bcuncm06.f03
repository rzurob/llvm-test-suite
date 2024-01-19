! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : May. 24, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test bind(c) common blocks work as
!*                              : global variables.
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine fsub()
implicit none
integer x
common /blk/ x
bind(c) :: /blk/
print *, x
x = 2
print *, x
end subroutine
