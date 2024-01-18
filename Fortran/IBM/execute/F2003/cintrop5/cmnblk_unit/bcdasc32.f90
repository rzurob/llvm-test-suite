! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f mod.mod; ${TR_SRC}/bcda.sh bcdasc32
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
module mod
implicit none
bind(c) /blk/
end module
subroutine sub
use mod
implicit none
integer x
common /blk/ x
bind(c) /blk/
end subroutine
program bcdasc32
end program
