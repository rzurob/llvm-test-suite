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
!* DESCRIPTION                  : Test the binding label can not be
!*                              : specified for other global variables.
!*                              : e.g. module name.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod1
integer, bind(c) :: x
end module
module mod2
real, bind(c, name="mod1") :: y
end module
use mod1
use mod2
end
