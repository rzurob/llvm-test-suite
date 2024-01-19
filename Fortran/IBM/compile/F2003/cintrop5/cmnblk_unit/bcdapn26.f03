! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : July 28, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test the binding label can not be
!*                              : specified for other global variables.
!*                              : e.g. program name.
!*                              : Test with -qmixed or without -qmixed
!*                              : Without -qmixed, all other global
!*                              : entities' name is ignoring differences
!*                              : in case.
!*                              : With -qmixed, FE interprets all names
!*                              : in lower case.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
integer x
common /prg/ x
bind(c) /prg/
end module
program Prg
use mod
end program
