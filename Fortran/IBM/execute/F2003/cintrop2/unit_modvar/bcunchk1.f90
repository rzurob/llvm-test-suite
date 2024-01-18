! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/unextchk.sh bcunchk1 csubext1
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
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
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
print *, ci
print *, cf
print *, ca
print *, cc
call csubext
print *, ci
print *, cf
print *, ca
print *, cc
end
