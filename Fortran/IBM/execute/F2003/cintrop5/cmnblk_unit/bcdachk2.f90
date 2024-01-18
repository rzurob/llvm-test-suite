! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/unextchk.sh bcdachk2 comc3
! %COMPOPTS: -qfree=f90 -qextchk  -qsuppress=1586-347
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
program bcdachk2
implicit none
integer x
common /blk/ x
bind(c) :: /blk/
x = 1
call csub()
end program
