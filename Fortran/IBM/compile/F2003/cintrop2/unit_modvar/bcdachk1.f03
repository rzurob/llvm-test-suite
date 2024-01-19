! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : May. 24, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90 -qextchk
!*
!* DESCRIPTION                  : Test hash string with -qextchk
!*                              : for bind(c) variables.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c) :: x
end module
module mod
real, bind(c) :: x
end module
use mod
x = 1
end
