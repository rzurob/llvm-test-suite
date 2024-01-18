!***********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: sign09.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: diff sign09.err $TR_SRC/sign09.vf;
! %END
!***********************************************************************
!*
!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SIGN= specifier
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SIGN
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  DESCRIPTION                : Testing the error messages of the
!*                               sign= specifier used in RTE
!*                             1 The sign= specifier in OPEN statement
!*                               for unformatted connection
!*                             2 The invalid sign= value in OPEN
!*                             3 The invalid sign= value in WRITE
!*                             4 The invalid sign= value for namelist output
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
character(20):: ch, smode, m, n
character (500) ::a(3)
namelist /nl/ m, n
ch="unformatted"
smode="aaaa"
m="AB"
n="CD"

!* The sign= used in unformatted output
open (1, file = "sign10.out", form=ch, sign="suppress")
close (1)

!* The invalid sign= value in OPEN
open (1, file = "sign10.out", sign=smode)

!* The invalid sign= value in WRITE
write (1, *, sign=smode) m

!* The invalid sign= value in namelist output
write (1, nl, sign=smode)
close(1)

write (a, nl, sign=smode)

end
