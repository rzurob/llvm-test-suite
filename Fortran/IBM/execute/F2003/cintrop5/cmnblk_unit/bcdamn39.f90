! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcdamix.sh bcdamn39
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : bcdamn39.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : July 28, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test the binding label can not be
!*                              : specified for other global variables.
!*                              : e.g. module name.
!*                              :
!*                              : Test with -qmixed or without -qmixed
!*                              :
!*                              : Without -qmixed, all other global
!*                              : entities' name is ignoring differences
!*                              : in case.
!*                              :
!*                              : With -qmixed, FE interprets all names
!*                              : in lower case.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
module Mod
end module
subroutine sub
use Mod
implicit none
integer x
common /blk/ x
bind(c, name="Mod") /blk/
end subroutine
program bcdamn39
end program
