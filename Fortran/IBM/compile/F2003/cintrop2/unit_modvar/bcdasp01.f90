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
!* DESCRIPTION                  : Test the bind(c) attribute can only
!*                              : be specified in specification part.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
contains
subroutine sub
implicit none
integer, bind(c) :: x
end subroutine
end module
