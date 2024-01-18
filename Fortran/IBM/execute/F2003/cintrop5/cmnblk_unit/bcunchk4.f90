! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/unextchk.sh bcunchk4 comc3
! %COMPOPTS: -qfree=f90 -qextchk -qsuppress=1586-347
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
program bcunchk4
integer x
common /bar/ x
bind(c, name='blk') :: /bar/
x = 1
print *, x
call csub()
print *, x
end program
