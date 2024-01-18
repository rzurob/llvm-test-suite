! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcda.sh bcdaar02
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
!* TEST CASE TITLE              : bcdaar02.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : May. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    : 
!*
!* DESCRIPTION                  : Test the bind(c) attribute conflicts
!*                              : with the size-zero array.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
bind(c) :: x
integer :: x(1:0)
end module
