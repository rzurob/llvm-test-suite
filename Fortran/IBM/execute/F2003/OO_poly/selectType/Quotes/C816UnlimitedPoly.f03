! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C816
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
!*    The selector is an unlimited poly entity
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C816UnlimitedPoly
  IMPLICIT REAL(8)(R), INTEGER(1)(I), CHARACTER(3)(C), COMPLEX(X), DOUBLE PRECISION(D), LOGICAL(4)(L)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg

  SELECT TYPE ( Arg )

    TYPE IS (INTEGER(1))
      STOP 50
    TYPE IS (INTEGER(2))
      STOP 51
    TYPE IS (INTEGER(4))
      STOP 52
    TYPE IS (INTEGER(8))
      STOP 53

    TYPE IS (REAL(4))
      STOP 60
!   TYPE IS (REAL(8))
!     STOP 61
    TYPE IS (REAL(16))
      STOP 62

    TYPE IS (LOGICAL(1))
      STOP 70
    TYPE IS (LOGICAL(2))
      STOP 71
    TYPE IS (LOGICAL(4))
      STOP 72
    TYPE IS (LOGICAL(8))
      STOP 73

    TYPE IS (CHARACTER(*))
      STOP 80
    TYPE IS (DOUBLE PRECISION)
      STOP 81

    TYPE IS (COMPLEX(16))
      STOP 84

  END SELECT

  END SUBROUTINE

  END

