!***********************************************************************
!*
!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : SIGN= specifier
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SIGN
!*  DESCRIPTION                : Testing the interaction of the SIGN=
!*                               specifier with S/SS/SP edit descriptors
!*                               in OPEN statement
!*                             1 The sign= specifier in OPEN statement
!*                               set the default sign mode in the following
!*                               connection
!*                             2 The S/SS/SP edit descriptors in WRITE statement
!*                               will temporarily change the SIGN mode
!*                               set in the OPEN statement
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
integer m
real    r
m = 20
r = 10.5
open (1, file = "sign04.out", sign = 'plus')
write(1, 10) m, r
write(1, 20) m, r
write(1, 30) m, r
write(1, 40) m, r
10 format (I3, " ",F5.1)
20 format (ss, I3, " ",F5.1)
30 format (sp, I3, " ",F5.1)
40 format (s, I3, " ",F5.1)
close (1)
end
