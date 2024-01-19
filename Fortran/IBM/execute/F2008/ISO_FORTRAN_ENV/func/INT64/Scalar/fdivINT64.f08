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
!*   This test case uses INT64 from IOS_FORTRAN_ENV module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fdivINT64
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

INTEGER(8) ::  x
INTEGER(8) ::  result
INTEGER(INT64) :: j
INTEGER(KIND=INT64) :: z

x=384244854775800
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
