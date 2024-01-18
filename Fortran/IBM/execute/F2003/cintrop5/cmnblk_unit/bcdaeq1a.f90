! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcda.sh bcdaeq1a
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
!* DESCRIPTION                  : Test bind(c) common block members
!*                              : should not be appeared as members
!*                              : of equivalence statements.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
integer x, a(2)
bind(c) /blk/
equivalence (x, a(1))
common /blk/ x
end module

program bcdaeq1a
end program
