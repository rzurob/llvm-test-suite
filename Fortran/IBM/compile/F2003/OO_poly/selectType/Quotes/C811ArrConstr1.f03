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
!*    The selector is an array constructor without ssociate-name =>
!*    (Wrong semantic check)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C811ArrConstr1
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr

  ALLOCATE(Ptr, SOURCE=1_1 )

  SELECT TYPE ( (/Ptr/) )
    TYPE IS (INTEGER(1))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  END

