! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/unextchk.sh bcunchk6 comain21
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
!* DATE                         : July 28, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common blocks work with
!*                              : -qextchk
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine fsub()
implicit none
integer x
common /blk/ x
bind(c) :: /blk/
print *, x
x = 2
print *, x
end subroutine
