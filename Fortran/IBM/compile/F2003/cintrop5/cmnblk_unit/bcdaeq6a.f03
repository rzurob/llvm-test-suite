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
subroutine sub
implicit none
integer :: x, a(2)
bind(c) /blk/
common /blk/ x
equivalence (x, a(1))
end subroutine
program bcdaeq6a
end program