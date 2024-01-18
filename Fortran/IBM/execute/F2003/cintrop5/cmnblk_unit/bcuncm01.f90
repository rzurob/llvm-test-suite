! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/uncs.sh bcuncm01 comc1
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
program bcuncm01
implicit none
integer x
real y
character(1) z
common /blk/ x, y, z
bind(c) :: /blk/
x = 1
y = 2.0
z = 'F'
print *, x, y, z
call csub()
print *, x, y, z
end program
