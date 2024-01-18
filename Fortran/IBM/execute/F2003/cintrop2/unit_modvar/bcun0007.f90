! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/uncs.sh bcun0007
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
!* DATE                         : May. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test bind(c) variables work as
!*                              : global variables.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character*11 :: ffh/'hello world'/
character, bind(c, name="ch") :: fh/'F'/
!complex*8 :: ffm = (3.4, 1.2)
!complex(4), bind(c, name="cm") :: fm = (1.2, 3.4)
real :: fff = 9.9
real, bind(c, name="cf") :: ff = 8.8
integer :: ffi = 11
integer, bind(c, name="ci") :: fi = 1
integer :: ffa(3,2,1) = 33
integer, bind(c, name="ca") :: fa(3,2,1) = 3
character(11) :: fffh/'HELLO WORLD'/
end module

use mod
print *, ffh
print *, fh
!print *, ffm
!print *, fm
print *, fff
print *, ff
print *, ffi
print *, fi
print *, ffa
print *, fa
print *, fffh
end
