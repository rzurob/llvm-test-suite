! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 15, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!*
!*   The selector is a poly allocatable var.
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = 2
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = 1
    END FUNCTION

  END MODULE

  PROGRAM SltVarAlloc
  USE M
  IMPLICIT NONE

  CLASS(*), ALLOCATABLE :: Var

  ALLOCATE(Child :: Var)

  SELECT TYPE ( Var )
    CLASS DEFAULT

      ASSOCIATE ( As => Var )
      SELECT TYPE (As)
        CLASS DEFAULT
          STOP 20
        TYPE IS (CHARACTER(*))
          STOP 21
        TYPE IS (Base)
          STOP 22
        CLASS IS (Base)
          STOP 23
        CLASS is (Child)
          STOP 24
        TYPE IS (Child)
          IF ( As%Base%GetId() .NE. 1 ) STOP 34
          IF ( As%GetId()      .NE. 2 ) STOP 35
      END SELECT
      END ASSOCIATE

  END SELECT

  END
