! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcda.sh bcdaza02
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
!* TEST CASE TITLE              : bcdaza02.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : Sept. 18, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common block members
!*                              : should not be specified as
!*                              : zero-sized array.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdaza02
implicit none
integer x(1,0)
common /blk/ x
bind(c) /blk/
end program
