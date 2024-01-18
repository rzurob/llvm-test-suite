! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/uncm.sh bcuncm16 comain2
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
!* DESCRIPTION                  : Test bind(c) common blocks in module
!*                              : work as global variables.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
integer x
common /blk/ x
bind(c) :: /blk/
end module
subroutine fsub()
use mod
implicit none
print *, x
x = 2
print *, x
end subroutine
