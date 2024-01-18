!***********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -rf sign04.out
! %COMPOPTS: -qfree=f90
! %GROUP: sign04.f
! %VERIFY: sign04.vf:sign04.out
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
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
integer m(2,4)
real    r(2,4)
integer i, j

do i = 1, 4
   do j = 1, 2
      m(j, i) = (i - 1) * 2 + j
      r(j, i) = (1.0 / m(j, i))
   end do
end do

open (1, file = "sign14.out", sign = 'plus')
write(1, 10) m, r
write(1, 20) m, r
write(1, 30) m, r
write(1, 40) m, r
10 format (8I3, " ",8F5.1)
20 format (ss, 8I3, " ",8F5.1)
30 format (sp, 8I3, " ",8F5.1)
40 format (s, 8I3, " ",8F5.1)
close (1)
end
