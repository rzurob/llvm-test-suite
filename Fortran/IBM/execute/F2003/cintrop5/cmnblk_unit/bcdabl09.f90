! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcda.sh bcdabl09
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
!* TEST CASE TITLE              : bcdabl09.f
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
!* DESCRIPTION                  : Test one binding label is specified
!*                              : for more than one common.
!*                              :
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdabl09
implicit none
integer :: i
common /foo/ i
real :: m
common /bar/ m
bind(c, name='flurb') :: /foo/, /bar/
end program
