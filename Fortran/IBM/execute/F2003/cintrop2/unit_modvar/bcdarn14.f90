! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcdamix.sh bcdarn14
! %COMPOPTS: -qfree=f90 -qsmp
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : July. 18, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test the binding label can not be
!*                              : specified for other global variables.
!*                              : e.g. critical lock name.
!*                              : Without -qmixed, all other global
!*                              : entities' name is ignoring differences
!*                              : in case.
!*                              : With -qmixed, FE interprets all names
!*                              : in lower case.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
integer, bind(c, name="Lock") :: x
end module
program bcdarn14
use mod
implicit none
!smp$ critical(lock)
x = 1
!smp$ end critical(lock)
end program
