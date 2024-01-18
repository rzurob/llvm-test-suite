! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/lagchk.sh bcdalag1
! %COMPOPTS: -qfree=f90 -qlanglvl=95std
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
!* TEST CASE TITLE              : bcdalag1.f
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
!* REQUIRED COMPILER OPTIONS    : 
!*                              :
!*
!* DESCRIPTION                  : Language level checking for BIND(C)
!*                              : attribute.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c) :: x
end module
program bcdalag1
end program
