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
!* DESCRIPTION                  : Test bindling label is unique.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod1
integer, bind(c, name="Foo") :: x
end module
module mod2
use mod1
integer, bind(c, name="Foo") :: y
end module
