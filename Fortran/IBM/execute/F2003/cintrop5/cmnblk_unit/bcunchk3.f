! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/unextchk.sh bcunchk3 comc31
! %COMPOPTS: -qfree=f90 -qextchk
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
!* TEST CASE TITLE              : bcunchk3.f
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
program bcunchk3
implicit none
integer x 
common /blk/ x
bind(c) :: /blk/
x = 1
print *, x
call csub()
print *, x
end program
