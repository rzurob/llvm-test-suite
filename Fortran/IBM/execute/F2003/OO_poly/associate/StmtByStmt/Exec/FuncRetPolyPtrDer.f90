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
!*    The selector is a func call returning a poly pointer
!*    of derived type
!*    (Comp Failed)
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

  PROGRAM FunRetPolyPtrDer
  USE M
  TYPE(Child), TARGET :: V = Child(BaseId= -1, ChildId=-2 )

  ASSOCIATE ( As => Func( V ) )

    IF ( As%GetID() .NE. -2) ERROR STOP 50
    IF ( As%BaseId  .NE. -1) ERROR STOP 51

    ASSOCIATE ( As1 => As%GetId() )
       IF ( As1 .NE. -2) ERROR STOP 52
    END ASSOCIATE

    IF ( .NOT. SAME_TYPE_AS(As, Child()) )     ERROR STOP 53
    IF ( .NOT. SAME_TYPE_AS(As%BaseComp, As) ) ERROR STOP 54

    IF ( As%BaseComp%BaseId  .NE. -1)  ERROR STOP 56
    IF ( As%BaseComp%GetId() .NE. -2)  ERROR STOP 57

    SELECT TYPE (As => As%BaseComp)
      TYPE IS (Child)
        IF ( As%GetId() .NE. -2 ) ERROR STOP 58
        IF ( As%ChildId .NE. -2 ) ERROR STOP 59
      CLASS DEFAULT
        STOP 77
    END SELECT

  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child), TARGET  :: Arg
    CLASS(Base), POINTER  :: Func

    Func => Arg
    FUNC%BaseComp => Func

  END FUNCTION

  END
