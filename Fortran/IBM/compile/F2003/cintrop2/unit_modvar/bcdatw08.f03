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
!* DESCRIPTION                  : Test the bind(c) attribute should
!*                              : not specified twice for same variable.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod1
integer, bind(c, name="foo") :: x
end module
module mod2
use mod1
bind(c, name="foo1") :: x
end module