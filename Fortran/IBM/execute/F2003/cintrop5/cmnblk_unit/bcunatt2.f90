! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcunext.sh bcunatt2
! %COMPOPTS: -qfree=f90 -qattr -qxref=full -qlist
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
!* TEST CASE TITLE              : bcunatt2.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : Sept. 08, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90_r
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common block work with
!*                              : -qattr
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
integer x
real y
common /blk/ x, y
bind(c, name="foo") /blk/
end module
program bcunatt2
end program
