! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/uncs.sh bcun0001 csub
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
!* DESCRIPTION                  : Test bind(c) variables work as
!*                              : global variables.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character ch
bind(c) ch
!complex(4), bind(c) :: cm
real, bind(c) :: cf
integer :: ci, ca(3,2,1)
bind(c) :: ci, ca
end module

use mod
ch = 'F'
!cm = (1.2, 3.4)
cf = 8.8
ci = 1
ca = 3
print *, "Variable ch, cm, cf, ci and ca are initialized in Fortran."
print *, ch
!print *, cm
print *, cf
print *, ci
print *, ca
call csub()
print *, "Variable ch, cm, cf, ci and ca are changed in C function csub()."
print *, "Now in Fortran:"
print *, ch
!print *, cm
print *, cf
print *, ci
print *, ca
end
