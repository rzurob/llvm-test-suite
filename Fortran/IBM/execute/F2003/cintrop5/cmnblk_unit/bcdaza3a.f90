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
integer x(1,0)
bind(c) /blk/
common /blk/ x
end subroutine

program bcdaza3a
end program
