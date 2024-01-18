! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcda.sh bcunchk4
! %COMPOPTS: -qfree=f90 -qextchk -qnodbg
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
!* REQUIRED COMPILER OPTIONS    : -qfree=f90 -qextchk
!*
!* DESCRIPTION                  : Test bind(c) variables work with
!*                              : -qextchk.
!*                              : For internal language checking.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c) :: ci
real, bind(c) :: cf
integer, bind(c) :: ca(2)
character, bind(c) :: cc
end module
module mod
integer, bind(c) :: ci
real, bind(c) :: cf
integer, bind(c) :: ca(2)
character, bind(c) :: cc
end module
use mod
ci = 1
cf = 2
ca = 3
cc = 'F'
end
