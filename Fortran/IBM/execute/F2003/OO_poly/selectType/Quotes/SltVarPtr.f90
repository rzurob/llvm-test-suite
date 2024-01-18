! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 16, 2004
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
!*   The selector is a poly pointer var.
!*    (Wrong result:297129 )
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero
    END TYPE

    TYPE, EXTENDS(Zero) :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM SltVarPtr
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr

  ALLOCATE(Child :: Ptr)
  Ptr => Ptr

  SELECT TYPE ( Ptr )
    CLASS DEFAULT

      ASSOCIATE ( As => Ptr  )
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
          IF ( As%Base%GetId() .NE. 1 ) ERROR STOP 34
          IF ( As%GetId()      .NE. 2 ) ERROR STOP 35
      END SELECT
      END ASSOCIATE

  END SELECT

  END
