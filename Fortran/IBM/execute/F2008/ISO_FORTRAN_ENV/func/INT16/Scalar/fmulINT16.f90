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
!*   This test case uses INT16 from IOS_FORTRAN_ENV module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fmulINT16
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

INTEGER(2) ::  x
INTEGER(2) ::  result
INTEGER(INT16) :: j
INTEGER(KIND=INT16) :: z,n

x=20
j=11
z=-120
n=-90

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
