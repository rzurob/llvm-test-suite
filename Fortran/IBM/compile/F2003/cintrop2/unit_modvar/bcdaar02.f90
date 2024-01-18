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
!* DESCRIPTION                  : Test the bind(c) attribute conflicts
!*                              : with the size-zero array.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
bind(c) :: x
integer :: x(1:0)
end module
