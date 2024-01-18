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
!*    The selector is a func call returning a value of derived type
!*    (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      CLASS(Child),  POINTER :: ChildComp  => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM FunRetDer
  USE M

  ASSOCIATE ( As => Func(Child(BaseId= -1, ChildId=-2 )) )
    IF ( As%GetID() .NE. -2) ERROR STOP 50
    IF ( As%BaseId  .NE. -1) ERROR STOP 51
    IF ( As%ChildId .NE. -2) ERROR STOP 61

    ASSOCIATE ( As1 => As%GetId() )
       IF ( As1 .NE. -2) ERROR STOP 52
    END ASSOCIATE

    IF ( .NOT. SAME_TYPE_AS(As, Child()) ) ERROR STOP 53
    IF ( As%ChildComp%BaseId  .NE. 1)      ERROR STOP 54
    IF ( As%ChildComp%GetId() .NE. 2)      ERROR STOP 55
  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child) :: Arg
    TYPE(Child)  :: Func

    Func = Arg
    ALLOCATE(FUNC%ChildComp)

  END FUNCTION

  END
