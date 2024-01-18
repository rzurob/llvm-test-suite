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
integer, bind(c) :: ca(2)
real, bind(c) :: caa(2,3), caaa(3,2,1)
end module
use mod
ca = 1
caa = 2
caaa = 3
print *, ca
print *, caa
print *, caaa
call csubext_arr
print *, ca
print *, caa
print *, caaa
end
