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
!* DESCRIPTION                  : Test variables with the bind(c)
!*                              : attribute must be some interoperable
!*                              : type with C.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
character(10), bind(c) :: x
end module
