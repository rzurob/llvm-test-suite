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
!*   The selector is an unlimited  poly pointer
!*    ()
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

  PROGRAM SltVarULPtr
  USE M
  IMPLICIT  NONE

  CLASS(*), POINTER  :: Ptr , Vtype

  ALLOCATE(Vtype, SOURCE=Child(BaseId=-1, ChildId=-2))

  SELECT TYPE ( AA => Vtype)
    CLASS DEFAULT

    ! ALLOCATE(Vtype, SOURCE=Child(BaseId=-1, ChildId=-2)) ! wrong! type changed inside
      ALLOCATE(Ptr, SOURCE=AA)

      SELECT TYPE ( As => Ptr )
        CLASS DEFAULT
          STOP 20
        CLASS is (Base)
          STOP 23
        TYPE is (INTEGER(1))
          STOP 24
        CLASS is (Child)
          STOP 25
        TYPE is (Child)
          SELECT TYPE (Ptr)
          CLASS IS (Child)
            IF ( Ptr%BaseId       .NE. -1 ) ERROR STOP 31
            IF ( Ptr%ChildId      .NE. -2 ) ERROR STOP 32
            IF ( Ptr%Base%GetId() .NE. -1 ) ERROR STOP 33
            IF ( Ptr%GetId()      .NE. -2 ) ERROR STOP 34

            Ptr%BaseId = 1
            Ptr%ChildId = 2

            IF ( As%BaseId       .NE. 1 ) ERROR STOP 31
            IF ( As%ChildId      .NE. 2 ) ERROR STOP 32
            IF ( As%Base%GetId() .NE. 1 ) ERROR STOP 33
            IF ( As%GetId()      .NE. 2 ) ERROR STOP 34
      END SELECT
    END SELECT
  END SELECT

  END