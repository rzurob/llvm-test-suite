! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcda.sh bcdasc23
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
!* DATE                         : June. 20, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common must be specified
!*                              : as common block in same scope unit.
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdasc23
implicit none
bind(c) /blk/
contains
subroutine sub
implicit none
integer x
bind(c) /blk/
common /blk/ x
end subroutine
end program
