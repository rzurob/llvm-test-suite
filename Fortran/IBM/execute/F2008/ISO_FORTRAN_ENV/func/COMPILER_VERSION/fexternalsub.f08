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

IMPLICIT NONE

INTERFACE
 SUBROUTINE PrintCompilerVersion()
 END SUBROUTINE
END INTERFACE

call PrintCompilerVersion()

END PROGRAM

SUBROUTINE PrintCompilerVersion()
USE, INTRINSIC :: ISO_FORTRAN_ENV
  print*,COMPILER_VERSION()
END SUBROUTINE