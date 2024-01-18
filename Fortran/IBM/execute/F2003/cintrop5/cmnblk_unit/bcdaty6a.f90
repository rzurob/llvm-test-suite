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
!*                              : should not be specified with non
!*                              : -interoperable data type.
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine sub
implicit none
bind(c) /blk/
common /blk/ x
character(10) :: x
end subroutine
program bcdaty6a
end program
