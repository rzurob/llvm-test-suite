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
!*    The selector is a func call returning an allocatable of derived type
!*    (Comp Failed)
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
      CLASS(Base),  ALLOCATABLE :: BaseArr(:)
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
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM FunRetAllocDer
  USE M
  TYPE(Child) :: V = Child(BaseId= -1, ChildId=-2, BaseArr=NULL() )
  ASSOCIATE ( As => Func( V ) )
    IF ( As%GetID() .NE. -2) ERROR STOP 50
    IF ( As%BaseId  .NE. -1) ERROR STOP 51
    IF ( As%ChildId .NE. -2) ERROR STOP 52

    ASSOCIATE ( As1 => As%GetId() )
       IF ( As1 .NE. -2) ERROR STOP 52
    END ASSOCIATE

    IF ( .NOT. SAME_TYPE_AS(As, Child( BaseArr=NULL())) )    ERROR STOP 53
    IF ( .NOT. SAME_TYPE_AS(As%BaseArr, As) ) ERROR STOP 54

    IF ( ANY(UBOUND(As%BaseArr) .NE. 3))     ERROR STOP 55
    IF ( As%BaseArr(1)%BaseId   .NE. 1)      ERROR STOP 56
    IF ( ANY(As%BaseArr%GetId() .NE. 2))     ERROR STOP 57
  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child)              :: Arg
    TYPE(Child), ALLOCATABLE  :: Func

    ALLOCATE(Func)
    Func = Arg
    ALLOCATE(Child :: FUNC%BaseArr(3))

  END FUNCTION

  END
