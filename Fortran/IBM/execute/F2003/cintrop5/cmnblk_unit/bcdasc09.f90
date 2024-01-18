! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcda.sh bcdasc09
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
!* DESCRIPTION                  : Test if a common block has the BIND(C)
!*                              : attribute, then it must be declared to
!*                              : have the BIND(C) attribute with the same
!*                              : binding label in all scoping unites
!*                              : in which it is declared.
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdasc09
implicit none
integer x
bind(c, name="first") /blk/
common /blk/ x
contains
subroutine sub
implicit none
integer y
bind(c, name="second") /blk/
common /blk/ y
end subroutine
end program
