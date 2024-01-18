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
!* DESCRIPTION                  : Test bind(c) variables work with
!*                              : -qextchk.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c) :: ci
real, bind(c) :: cf
integer, bind(c) :: ca(2)
character, bind(c) :: cc
end module
module mod
real, bind(c) :: ci
integer, bind(c) :: cf
real, bind(c) :: ca(2)
integer, bind(c) :: cc
end module
use mod
ci = 1
cf = 2
ca = 3
cc = 4
end
