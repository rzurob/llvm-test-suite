! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/uncs.sh bcunlab3 cssub
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
!* TEST CASE TITLE              : bcunlab3.f
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
!* DESCRIPTION                  : Test the leading and trailing space 
!*                              : will be ignored for binding label.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c, name='f'//'o'//'o1') :: x = 1
real, bind(c, name=('f')//('oo'//'2')) :: y = 2
character*1, bind(c, name = ( 'f' // ( 'o' // ( 'o' // ( '3' ) ) ) ) ) :: z /'F'/
end module
use mod
print *, x, y, z
call cssub()
print *, x, y, z
end
