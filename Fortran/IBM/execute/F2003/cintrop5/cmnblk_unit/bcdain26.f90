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
!*                              : e.g. external procedure name defined
!*                              : in interface body.
!*                              : Test with -qmixed or without -qmixed
!*                              : Without -qmixed, all other global
!*                              : entities' name is ignoring differences
!*                              : in case.
!*                              : With -qmixed, FE interprets all names
!*                              : in lower case.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
interface
subroutine sub
end subroutine
end interface
integer x
common /blk/ x
bind(c, name="Sub") /blk/
end module
program bcdain26
end program
