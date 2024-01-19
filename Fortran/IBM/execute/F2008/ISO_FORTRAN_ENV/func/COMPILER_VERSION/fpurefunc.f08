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
!*   print the compiler version from a pure function
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
PROGRAM finternalsub
USE, INTRINSIC :: ISO_FORTRAN_ENV

IMPLICIT NONE

CHARACTER,POINTER :: result
INTEGER :: funcres

INTERFACE
 PURE FUNCTION PrintCompilerVersion()
  CHARACTER(100):: PrintCompilerVersion
 END FUNCTION
END INTERFACE

print*,PrintCompilerVersion()
END PROGRAM


PURE FUNCTION PrintCompilerVersion()
 USE, INTRINSIC :: ISO_FORTRAN_ENV
 CHARACTER(100):: PrintCompilerVersion
  PrintCompilerVersion=COMPILER_VERSION()
END FUNCTION
