! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Sept. 18, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common block members
!*                              : should not be specified as
!*                              : zero-sized array.
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine sub
implicit none
common /blk/ x
bind(c) /blk/
integer x(1,0)
end subroutine

program bcdaza06
end program
