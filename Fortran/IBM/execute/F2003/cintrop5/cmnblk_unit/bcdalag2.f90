! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/lagchk.sh bcdalag2
! %COMPOPTS: -qfree=f90 -qlanglvl=90std
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
!* TEST CASE TITLE              : bcdalag2.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : July. 28, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    :
!* DESCRIPTION                  : Language level checking for BIND(C)
!*                              : attribute.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdalag2
implicit none
integer x
bind(c) :: /blk/
common /blk/ x
end program
