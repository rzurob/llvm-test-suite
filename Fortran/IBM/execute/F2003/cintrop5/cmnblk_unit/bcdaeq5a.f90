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
!*                              : should not be appeared as members
!*                              : of equivalence statements.
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdaeq5a
implicit none
integer x, a(2)
bind(c) /blk/
common /blk/ x
equivalence (x, a(1))
end program
