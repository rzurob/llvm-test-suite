!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : fmulREAL128
!*
!*  DATE                       : 2010-07-12
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*   This test case uses REAL128 from IOS_FORTRAN_ENV module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fmulREAL128
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

REAL(16) ::  x
REAL(16) ::  result
REAL(REAL128) :: j
REAL(KIND=REAL128) :: z,n

x=0.000693Q+208
j=0.000002Q+108
z=-0.520693Q-008
n=-0.000319Q-012

IF (KIND(x) .NE. KIND(j)) THEN
 STOP 1
END IF

IF (KIND(x) .NE. KIND(z)) THEN
 STOP 2
END IF

result = x * z

PRINT*,x,"*",z,"=",result

result = j * z

PRINT*,j,"*",z,"=",result

result = x * j * n

PRINT*,x,"*",j,"*",n,"=",result

END PROGRAM
