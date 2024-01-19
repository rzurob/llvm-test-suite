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
!*                              : e.g. external procedure name defined
!*                              : inside interface.
!*                              : Without -qmixed, all other global
!*                              : entities' name is ignoring differences
!*                              : in case.
!*                              : With -qmixed, FE interprets all names
!*                              : in lower case.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
interface
subroutine blk
end subroutine
end interface
integer, bind(c, name="Blk") :: x
end module
program bcdain15
end program
