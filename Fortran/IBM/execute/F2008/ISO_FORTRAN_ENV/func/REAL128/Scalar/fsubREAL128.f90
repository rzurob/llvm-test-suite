!*******************************************************************************
!*  ============================================================================
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

PROGRAM fsubREAL128
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

REAL(16) ::  x
REAL(16) ::  result
REAL(REAL128) :: j
REAL(KIND=REAL128) :: z

x=1.797693Q+308
j=0.225074Q+108
z=1.000000Q-199

IF (KIND(x) .NE. KIND(j)) THEN
 STOP 1
END IF

IF (KIND(x) .NE. KIND(z)) THEN
 STOP 2
END IF

result = x - z

PRINT*,x,"-",z,"=",result

result = j - z

PRINT*,j,"-",z,"=",result

result = x - j

PRINT*,x,"-",j,"=",result

END PROGRAM
