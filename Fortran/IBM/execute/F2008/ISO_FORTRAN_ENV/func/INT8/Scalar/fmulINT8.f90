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
!*   This test case uses INT8 from IOS_FORTRAN_ENV module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fmulINT8
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

INTEGER(1) ::  x
INTEGER(1) ::  result
INTEGER(INT8) :: j
INTEGER(KIND=INT8) :: z

x=3
j=9
z=10

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

result = x * j

PRINT*,x,"*",j,"=",result

END PROGRAM
