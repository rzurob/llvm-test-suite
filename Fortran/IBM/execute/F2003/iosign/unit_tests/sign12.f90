!***********************************************************************
!*
!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : SIGN= specifier
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SIGN
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  DESCRIPTION                : Testing the functionality of the
!*                               SIGN= specifier used in OPEN statement
!*                             1 The sign= specifier in OPEN statement
!*                               set the sign mode in the connection
!*                             2 The sign= specifier in WRITE statement
!*                               will temporarily change the SIGN mode
!*                               set in the OPEN statement
!*                             3 The SUPPRESS mode is the same as
!*                               PROCESSOR_DEFINED.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
integer*1   :: m1(2)
integer*4   :: m4(2)
integer*8   :: m8(2)
real*4      :: r4(2)
real*8      :: r8(2)
real*16     :: r16(2)
complex     :: c4(2)
complex*8   :: c8(2)
complex*16  :: c16(2)
m1  = 10
m4  = -40
m8  = 80
r4  = 4.4
r8  = 8.8
r16 = 16.16
c4  = (1.1, 2.2)
c8  = (-3.3, 4.4)
c16 = (5.5, -6.6)

open (1, file = "sign12.out", sign = 'plus')
write(1, 10) m1, m4, m8, r4, r8, r16, c4, c8, c16
write(1, 10, sign = 'suppress') m1, m4, m8, r4, r8, r16, c4, c8, c16
write(1, 10, sign = 'processor_defined') m1, m4, m8, r4, r8, r16, c4, c8, c16
10 format (6I5, " ",18F5.1)
close (1)
end
