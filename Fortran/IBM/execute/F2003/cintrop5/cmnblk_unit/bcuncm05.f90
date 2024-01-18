! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/uncm.sh bcuncm05 comain1
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
!* TEST CASE TITLE              : bcuncm05.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : May. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test bind(c) common blocks work as
!*                              : global variables.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine fsub()
implicit none
integer x
real y
character z
common /blk/ x, y, z
bind(c) :: /blk/
print *, x, y, z
x = 4
y = 5.0
z = 'F'
print *, x, y, z
end subroutine
