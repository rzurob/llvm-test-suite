! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : May. 24, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test the bind(c) attribute must not
!*                              : be specified for the common block
!*                              : members.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
common /blk/ x
integer, bind(c) :: x
end module