! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcda.sh bcdamix3
! %COMPOPTS: -qfree=f90 -qmixed
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
!* DATE                         : May. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bindling label work with
!*                              : -qmixed.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c, name="PRG") :: x
end module
program PRG
use mod
end program
