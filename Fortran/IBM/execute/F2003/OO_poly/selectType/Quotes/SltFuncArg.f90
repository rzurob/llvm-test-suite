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
!*   The selector is a function return with associate name as argument
!*    (297706)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero
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


  PROGRAM SltFuncArg
  USE M
  IMPLICIT NONE
  INTEGER :: Cnt=5

  ASSOCIATE( As => Fun(Child(ChildId=2, BaseId=1)))

  END ASSOCIATE


  CONTAINS


  RECURSIVE FUNCTION Fun(Arg)
  CLASS(Zero), TARGET ::Arg
  CLASS(Zero), POINTER :: Fun

  IF(Cnt .GT. 1 ) THEN
    Cnt = Cnt -1
    SELECT TYPE( AS => Fun(Arg))
    END SELECT
  ELSE

  ALLOCATE(Child :: Fun)

  SELECT TYPE ( As0 => Arg )
    CLASS IS (Zero)
      SELECT TYPE (As => As0)
        TYPE IS (Child)
          IF ( As%Base%GetId() .NE. 1 ) ERROR STOP 34
          IF ( As%GetId()      .NE. 2 ) ERROR STOP 35
          IF ( As%BaseId       .NE. 1 ) ERROR STOP 36
          IF ( As%ChildId      .NE. 2 ) ERROR STOP 37
          CALL As%SetId()
          CALL As%Base%SetId()
          IF ( As%Base%GetId() .NE. -1 ) ERROR STOP 34
          IF ( As%GetId()      .NE. -2 ) ERROR STOP 35
          IF ( As%BaseId       .NE. -1 ) ERROR STOP 36
          IF ( As%ChildId      .NE. -2 ) ERROR STOP 37
       CLASS DEFAULT
          STOP 40
      END SELECT

    TYPE is (Base)
      STOP 32
    TYPE IS (Zero)
      STOP 38

  END SELECT

  END IF

  END FUNCTION

  END

