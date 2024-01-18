! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/lagchk.sh bcdalag3
! %COMPOPTS: -qfree=f90 -qlanglvl=77std
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
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Language level checking for BIND(C)
!*                              : attribute.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c) :: x
end module
program bcdalag3
end program
