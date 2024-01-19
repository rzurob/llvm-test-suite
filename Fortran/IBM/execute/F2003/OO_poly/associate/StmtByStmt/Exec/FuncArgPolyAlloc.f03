! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 02, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The poly associating entity is used as actual argument
!*   The associated entity is poly allocatables
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
      CLASS(Base),  POINTER :: BaseComp => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      CLASS(Child),  POINTER :: ChildComp => NULL()
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

  PROGRAM FuncArgPolyAlloc
  USE M
  CLASS(*),     ALLOCATABLE :: V1
  CLASS(Base),  ALLOCATABLE :: V2
  CLASS(Child), ALLOCATABLE :: V3

  ALLOCATE(V3, SOURCE=Child(BaseId=-1, ChildId=-2))
  ALLOCATE(V2, SOURCE=V3)
  ALLOCATE(V1, SOURCE=V2)

  ASSOCIATE ( As => V1 )
  ASSOCIATE ( As1 => Func(As) )
    SELECT TYPE (As1)
      TYPE IS (Child)
        IF ( As1.GetId()      .NE. -2 ) ERROR STOP 30
        IF ( As1.Base%GetId() .NE. -1 ) ERROR STOP 31
      CLASS DEFAULT
        STOP 32
    END SELECT
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => V2 )
  ASSOCIATE ( As1 => Func(As) )
    SELECT TYPE (As1)
      TYPE IS (Child)
        IF ( As1.GetId()      .NE. -2 ) ERROR STOP 40
        IF ( As1.Base%GetId() .NE. -1 ) ERROR STOP 41
      CLASS DEFAULT
        STOP 42
    END SELECT
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => V3 )
  ASSOCIATE ( As1 => Func(As) )
    SELECT TYPE (As1)
      TYPE IS (Child)
        IF ( As1.GetId()      .NE. -2 ) ERROR STOP 50
        IF ( As1.Base%GetId() .NE. -1 ) ERROR STOP 51
      CLASS DEFAULT
        STOP 52
    END SELECT
  END ASSOCIATE
  END ASSOCIATE

  CONTAINS


  FUNCTION Func(Arg)
    CLASS(*), TARGET      :: Arg
    CLASS(*), ALLOCATABLE :: Func

    ALLOCATE(Func, SOURCE=Arg)

  END FUNCTION

  END
