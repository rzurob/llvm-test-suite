!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : fmulREAL32
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
!*   This test case uses REAL32 from IOS_FORTRAN_ENV module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fmulREAL32
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

REAL(4) ::  x
REAL(4) ::  result
REAL(REAL32) :: j
REAL(KIND=REAL32) :: z,n

x=0.000693E+22
j=0.000002E+18
z=-0.520693E-08
n=-0.000319E-12

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
