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
!*    Print the compiler version from an subroutine
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
PROGRAM finternalsub
USE, INTRINSIC :: ISO_FORTRAN_ENV

IMPLICIT NONE

call PrintCompilerVersion()

CONTAINS

SUBROUTINE PrintCompilerVersion()
  print*,COMPILER_VERSION()
END SUBROUTINE


END PROGRAM