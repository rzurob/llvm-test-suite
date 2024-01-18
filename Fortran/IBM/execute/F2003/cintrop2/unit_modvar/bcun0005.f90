! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/uncs.sh bcun0005 csub
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
!* TEST CASE TITLE              : bcun0005.f
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
!* DESCRIPTION                  : Test bind(c) variables work as
!*                              : global variables.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character*1, bind(c) :: ch/'F'/
!complex(4), bind(c) :: cm = (1.2, 3.4)
real, bind(c) :: cf = 8.8
integer, bind(c) :: ci = 1
integer, bind(c) :: ca(3,2,1) = 3
end module

use mod
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
