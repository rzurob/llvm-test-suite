! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcdamix.sh bcdacn13
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
!* TEST CASE TITLE              : bcdacn13.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : July. 18, 2003
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
!*                              : e.g. common block name.
!*                              : Without -qmixed, all other global
!*                              : entities' name is ignoring differences
!*                              : in case.
!*                              : With -qmixed, FE interprets all names
!*                              : in lower case.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
integer, bind(c, name="Blk") :: x
end module
program bcdacn13
use mod
implicit none
integer y
common /blk/ y
end program
