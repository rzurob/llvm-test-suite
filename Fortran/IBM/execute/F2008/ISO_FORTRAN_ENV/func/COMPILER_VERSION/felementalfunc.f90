!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : felementalfunc
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

CHARACTER(100) :: cmpVersion,res

INTERFACE
 ELEMENTAL FUNCTION PrintCompilerVersion(cmpVersion)
  CHARACTER(100),INTENT(IN):: cmpVersion
 CHARACTER(100):: PrintCompilerVersion
 END FUNCTION
END INTERFACE

 cmpVersion=COMPILER_VERSION()

 res=PrintCompilerVersion(cmpVersion)

 IF ( cmpVersion .EQ. res ) THEN
  print*,res
 END IF
END PROGRAM


ELEMENTAL FUNCTION PrintCompilerVersion(cmpVersion)
 USE, INTRINSIC :: ISO_FORTRAN_ENV
 CHARACTER(100),INTENT(IN):: cmpVersion
 CHARACTER(100):: PrintCompilerVersion
 CHARACTER(100):: CompilerVersion

 PrintCompilerVersion=COMPILER_VERSION()

END FUNCTION
