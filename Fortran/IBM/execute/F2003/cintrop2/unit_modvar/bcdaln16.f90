! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : July. 18, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test the binding label can not be
!*                              : specified for other global variables.
!*                              : e.g. another binding label
!*                              : Without -qmixed, all other global
!*                              : entities' name is ignoring differences
!*                              : in case.
!*                              : With -qmixed, FE interprets all names
!*                              : in lower case.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod1
implicit none
integer, bind(c) :: Mod
end module
module mod2
use mod1
implicit none
integer, bind(c, name="mod") :: x
end module
program bcdaln16
end program
