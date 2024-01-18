! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 05, 2005
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
!*   The selector is a function return with associate name(to allocatable) as argument
!*    (297701)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero
    CONTAINS
      PROCEDURE, NoPASS   :: ReturnObj
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    FUNCTION ReturnObj(Arg)
    CLASS(Zero) :: Arg
    CLASS(*), ALLOCATABLE :: ReturnObj
      ALLOCATE(ReturnObj, SOURCE=Arg)
    END FUNCTION

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base)  :: Arg
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child)  :: Arg
      Arg%ChildId = -Arg%ChildId
    END SUBROUTINE

  END MODULE


  PROGRAM SltFuncArgAlloc
  USE M
  IMPLICIT NONE
  CLASS(Zero), ALLOCATABLE, TARGET :: V

  ALLOCATE(Child :: V)

  SELECT TYPE ( As0 => V )
    CLASS IS (Zero)
      SELECT TYPE (As => As0%ReturnObj(As0))
        TYPE IS (Child)
          IF ( As%Base%GetId() .NE. 1 ) STOP 34
          IF ( As%GetId()      .NE. 2 ) STOP 35
          IF ( As%BaseId       .NE. 1 ) STOP 36
          IF ( As%ChildId      .NE. 2 ) STOP 37
          CALL As%SetId()
          CALL As%Base%SetId()
          IF ( As%Base%GetId() .NE. -1 ) STOP 34
          IF ( As%GetId()      .NE. -2 ) STOP 35
          IF ( As%BaseId       .NE. -1 ) STOP 36
          IF ( As%ChildId      .NE. -2 ) STOP 37
        CLASS DEFAULT
          STOP 40
      END SELECT

    TYPE is (Base)
      STOP 32
    TYPE IS (Zero)
      STOP 38
    CLASS DEFAULT
      STOP 39
  END SELECT

  END
