!***********************************************************************
!*
!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : SIGN=specifier in WRITE
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SIGN
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  DESCRIPTION                : Testing the functionality of the
!*                               SIGN= specifier used in WRITE statement
!*                             1 The PROCESSOR_DEFINED is the default
!*                               SIGN mode, which is the same as SUPPRESS
!*                             2 The sign= specifier in WRITE statement
!*                               will temporarily change the SIGN mode
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
real r
integer m
r = 10.5
m = 20
write(*, 10) m, r
write(*, 10, sign = 'plus') m, r
write(*, 10, sign = 'processor_defined') m, r
write(*, 10, sign = 'suppress') m, r
10 format (I3, " ", F5.1)
end
