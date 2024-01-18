! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/unextchk.sh bcdachk2 comc3
! %COMPOPTS: -qfree=f90 -qextchk  -qsuppress=1586-347
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
!* TEST CASE TITLE              : bcdachk2.f
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
!* DESCRIPTION                  : Test bind(c) common blocks work with
!*                              : -qextchk
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdachk2
implicit none
integer x 
common /blk/ x
bind(c) :: /blk/
x = 1
call csub()
end program
