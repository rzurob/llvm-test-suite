! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/unattr.sh bcunattr
! %COMPOPTS: -qfree=f90 -qattr -qlist -qxref=full
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
!* TEST CASE TITLE              : bcunattr.f
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
!* REQUIRED COMPILER OPTIONS    : -qfree=f90 -qattr -qlist -qxref=full
!*
!* DESCRIPTION                  : Test bind(c) variables work with
!*                              : -qattr.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character ch
bind(c) ch
!complex(4), bind(c) :: cm
real, bind(c) :: cf
integer :: ci, ca(3,2,1)
bind(c) :: ci, ca
end module

use mod
ch = 'F'
!cm = (1.2, 3.4)
cf = 8.8
ci = 1
ca = 3
end 
