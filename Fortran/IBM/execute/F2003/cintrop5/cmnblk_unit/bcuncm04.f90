! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/uncs.sh bcuncm04 comc3
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
program bcuncm04
integer x
common /bar/ x
bind(c, name='blk') :: /bar/
x = 1
print *, x
call csub()
print *, x
end program
