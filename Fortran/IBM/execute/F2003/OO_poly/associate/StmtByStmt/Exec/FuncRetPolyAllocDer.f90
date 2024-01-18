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
!*    The selector is a func call returning a poly allocatable
!*    of derived type
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

  PROGRAM FunRetPolyAllocDer
  USE M
  TYPE(Child) :: V = Child(BaseId= -1, ChildId=-2, BaseArr=NULL() )

  ASSOCIATE ( As => Func( V ) )
  SELECT TYPE( As )
  TYPE IS (Child)

    IF ( As%GetID() .NE. -2) STOP 50
    IF ( As%BaseId  .NE. -1) STOP 51

    ASSOCIATE ( As1 => As%GetId() )
       IF ( As1 .NE. -2) STOP 52
    END ASSOCIATE

    IF ( .NOT. SAME_TYPE_AS(As, Child(BaseArr=NULL())) )         STOP 53
    IF ( .NOT. SAME_TYPE_AS(As%BaseArr, As%Base) ) STOP 54

    IF ( ANY(LBOUND(As%BaseArr) .NE. 1))  STOP 55
    IF ( ANY(UBOUND(As%BaseArr) .NE. 3))  STOP 55
    IF ( As%BaseArr(1)%BaseId   .NE. 1)   STOP 56
    IF ( ANY(As%BaseArr%GetId() .NE. 1))  STOP 57

    IF ( As%GetId() .NE. -2 ) STOP 58
    IF ( As%ChildId .NE. -2 ) STOP 59

  CLASS DEFAULT
    STOP 60
  END SELECT
  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child)              :: Arg
    CLASS(Base), ALLOCATABLE  :: Func

    ALLOCATE(Child :: Func)

    SELECT TYPE(Func)
      TYPE IS (Child)
        Func = Arg
        ALLOCATE( FUNC%BaseArr(3))
      CLASS DEFAULT
        STOP 20
    END SELECT
  END FUNCTION

  END
