!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-07-12
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 376078
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*   prints an array containing compiler version information
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
PROGRAM fcompOptions
USE, INTRINSIC :: ISO_FORTRAN_ENV
IMPLICIT NONE

 CHARACTER(*), PARAMETER :: Options = COMPILER_OPTIONS()

 print*,Options

END PROGRAM
