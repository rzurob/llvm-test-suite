!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : fdivREAL32
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

PROGRAM fdivREAL32
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

REAL(4) ::  x
REAL(4) ::  result
REAL(REAL32) :: j
REAL(KIND=REAL32) :: z

x=1.797693E+10
j=4
z=2

IF (KIND(x) .NE. KIND(j)) THEN
 STOP 1
END IF

IF (KIND(x) .NE. KIND(z)) THEN
 STOP 2
END IF

result = x / z

PRINT*,x,"/",z,"=",result

result = j / z

PRINT*,j,"/",z,"=",result

result = x / j

PRINT*,x,"/",j,"=",result

END PROGRAM
