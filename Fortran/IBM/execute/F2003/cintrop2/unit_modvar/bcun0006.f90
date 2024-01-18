! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/uncs.sh bcun0006 csub
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
!* TEST CASE TITLE              : bcun0006.f
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
character, bind(c, name="ch") :: fh/'F'/
!complex(4), bind(c, name="cm") :: fm = (1.2, 3.4)
real, bind(c, name="cf") :: ff = 8.8
integer, bind(c, name="ci") :: fi = 1
integer, bind(c, name="ca") :: fa(3,2,1) = 3
end module

use mod
print *, "Variable fh, fm, ff, fi and fa are initialized in Fortran."
print *, fh
!print *, fm
print *, ff
print *, fi
print *, fa
call csub()
print *, "Variable fh, fm, ff, fi and fa are changed in C function csub()."
print *, "Now in Fortran:"
print *, fh
!print *, fm
print *, ff
print *, fi
print *, fa
end 
