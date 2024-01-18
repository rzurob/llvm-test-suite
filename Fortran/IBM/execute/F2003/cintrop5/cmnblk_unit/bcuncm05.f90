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
!*
!
!* DATE                         : May. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test bind(c) common blocks work as
!*                              : global variables.
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
