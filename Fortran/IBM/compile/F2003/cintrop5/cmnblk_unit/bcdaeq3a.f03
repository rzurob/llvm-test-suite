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
subroutine sub
implicit none
integer z, a(2)
bind(c) /blk/
equivalence (z, a(1))
common /blk/ z
end subroutine
program bcdaeq3a
end program
