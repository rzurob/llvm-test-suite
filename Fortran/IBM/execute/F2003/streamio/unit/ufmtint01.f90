!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: ufmtint01.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************

!*  ===================================================================
!*
!*  DATE                       : April 2003
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test unformatted stream IO of real values
!*
!234567890123456789012345678901234567890123456789012345678901234567890
integer*2 k(4)
integer*4 i(2)
integer*8 j

equivalence(i,j,k)
j = 0

open(11, access='stream', status='new') ! unformatted

! ascii 45@Z]etu == 0x3435405A5D657475
!                == 52 53 64 90 93 101 116 117
write(11) '45@Z]etu'
rewind 11

read(11) i
if (i(1) /= 875905114) error stop 1
if (i(2) /= 1566930037) error stop 2

rewind 11

read(11) j
if (j /= 3761983820596081781_8) error stop 3

rewind 11

read(11) k
if (k(1) /= 13365) error stop 4
if (k(2) /= 16474) error stop 5
if (k(3) /= 23909) error stop 6
if (k(4) /= 29813) error stop 7

close(11, status='delete')

end

