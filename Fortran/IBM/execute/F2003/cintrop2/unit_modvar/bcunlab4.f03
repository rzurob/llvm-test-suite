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
!* DESCRIPTION                  : Test binding label as constant.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character*4, parameter :: f1 = 'foo1', f2 = 'foo2', f3 = 'foo3'
integer, bind(c, name=f1) :: x = 1
real, bind(c, name=f2) :: y = 2
character, bind(c, name=f3) :: z/'F'/
end module
use mod
print *, x, y, z
call cssub()
print *, x, y, z
end
