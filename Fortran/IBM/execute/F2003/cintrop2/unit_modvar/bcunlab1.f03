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
!* DESCRIPTION                  : Test the leading and trailing space
!*                              : will be ignored for binding label.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c, name="  foo1") :: x = 1
real, bind(c, name="foo2  ") :: y = 2
character, bind(c, name="  foo3  ") :: z/'F'/
end module
use mod
print *, x, y, z
call cssub()
print *, x, y, z
end