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
!*                              : e.g. external procedure name.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c, name="sub") :: x
external sub
end module
program bcdaen04
end program
