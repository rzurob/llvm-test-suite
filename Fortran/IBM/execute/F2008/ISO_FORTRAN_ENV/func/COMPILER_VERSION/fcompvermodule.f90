!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : fcompvermodule
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
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
MODULE fcompvermodule
USE, INTRINSIC :: ISO_FORTRAN_ENV
IMPLICIT NONE



CONTAINS
 SUBROUTINE PrintCompilerVersion()
  print*,COMPILER_VERSION()
 END SUBROUTINE

END MODULE
