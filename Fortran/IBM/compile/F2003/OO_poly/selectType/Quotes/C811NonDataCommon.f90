! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C811
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a common block name.
!*    (Diagnostic)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C811NonDataInterface
  IMPLICIT NONE

  INTEGER :: A(100)
  COMMON /Block/ A

  SELECT TYPE ( As => Block )
    TYPE IS (INTEGER)
      STOP 50
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  END

