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
!*   This test case uses INT32 from IOS_FORTRAN_ENV module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM faddINT32
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

INTEGER(4) ::  x
INTEGER(4) ::  result
INTEGER(INT32) :: j
INTEGER(KIND=INT32) :: z

x=90999
j=49
z=900

IF (KIND(x) .NE. KIND(j)) THEN
 STOP 1
END IF

IF (KIND(x) .NE. KIND(z)) THEN
 STOP 2
END IF

result = x + z

PRINT*,x,"+",z,"=",result

result = j + z

PRINT*,j,"+",z,"=",result

result = x + j

PRINT*,x,"+",j,"=",result

END PROGRAM
