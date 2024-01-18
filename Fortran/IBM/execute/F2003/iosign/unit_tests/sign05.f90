!***********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -rf sign05.out
! %COMPOPTS: -qfree=f90
! %GROUP: sign05.f
! %VERIFY: sign05.vf:sign05.out
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
!*                               specifier with S/SP/SS edit descriptors
!*                               in WRITE statement
!*                              The S/SP/SS edit descriptors always win
!*                              over the sign= specifier in WRITE statement
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
integer m
real    r
m = 20
r = 10.5

write(*, *) "Testing ss overwrite sign=specifier mode"
write(*, 20, sign='plus') m, r
write(*, 20, sign='suppress') m, r
write(*, 20, sign='processor_defined') m, r

write(*, *) "Testing sp overwrite sign=specifier mode"
write(*, 30, sign='plus') m, r
write(*, 30, sign='suppress') m, r
write(*, 30, sign='processor_defined') m, r

write(*, *) "Testing s overwrite sign=specifier mode"
write(*, 40, sign='plus') m, r
write(*, 40, sign='suppress') m, r
write(*, 40, sign='processor_defined') m, r

10 format (I3, " ",F5.1)
20 format (ss, I3, " ",F5.1)
30 format (sp, I3, " ",F5.1)
40 format (s, I3, " ",F5.1)

end
