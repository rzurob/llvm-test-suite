! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcda.sh bcdaza4a
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
!* DATE                         : Sept. 18, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common block members
!*                              : should not be specified as
!*                              : zero-sized array.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
bind(c) /blk/
common /blk/ x
integer x(1,0)
end module

program bcdaza4a
end program
