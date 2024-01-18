! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C813
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
!*    The selector is a parameter  array of char
!*    Int/Real/Character/Complex/Double/LOGICAL
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C813IntrinTypes
  IMPLICIT REAL(8)(R), INTEGER(1)(I), CHARACTER(3)(C), COMPLEX(X), DOUBLE PRECISION(D), LOGICAL(4)(L)


  SELECT TYPE (  As => 1_8 )
    TYPE IS (INTEGER(8))
      STOP 50
  END SELECT

  SELECT TYPE (  As => C )
    TYPE IS (INTEGER(8))
      STOP 51
  END SELECT

  SELECT TYPE (  As => II )
    TYPE IS (INTEGER(8))
      STOP 52
  END SELECT

  SELECT TYPE (  As => 0.0D0 )
    TYPE IS (DOUBLE PRECISION)
      STOP 53
  END SELECT

  SELECT TYPE (  As => X )
    TYPE IS (COMPLEX(16))
      STOP 54
  END SELECT

  SELECT TYPE (  As => L )
    TYPE IS (LOGICAL(8))
      STOP 50
  END SELECT

  END

