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
!*    The selector is a type bound function call
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS :: SetId
      PROCEDURE, PASS   :: GetObj
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

    ELEMENTAL FUNCTION GetObj(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    TYPE (Child)    :: GetObj
      SELECT TYPE (Arg)
      TYPE IS (Child)
        GetObj = Arg
      TYPE IS (Base)
        GetObj%Base = Arg
      CLASS DEFAULT
      END SELECT
    END FUNCTION

    SUBROUTINE SetId(Obj, Id)
    CLASS(Base)  :: Obj(:,:)
    INTEGER      :: Id
      SELECT TYPE (Obj)
      TYPE IS (Base)
        Obj%BaseId = Id
      TYPE IS (Child)
        Obj%ChildId = Id
      END SELECT
    END SUBROUTINE

  END MODULE

  PROGRAM ArrTypBnd
  USE M
  IMPLICIT NONE
  INTEGER :: i
  CLASS(*), ALLOCATABLE :: Arr(:,:)

  ALLOCATE(Arr(5,5), SOURCE=Child(BaseID=-1, ChildID=-2))

  ASSOCIATE ( As => Arr(2:3, 4:5) )
  SELECT TYPE (As )
  CLASS IS ( Child)
  ASSOCIATE ( As => As(:,:)%GetObj() )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 30
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 32

    IF ( ANY (As%Base%GetID()  .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 33
    IF ( ANY (As%GetID()       .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 34

    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( ANY(As0 .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 41
       IF ( ANY(As1 .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 42
    END ASSOCIATE

    ASSOCIATE ( As2 => As%Base )
      IF ( ANY(As2%GetID() .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) STOP 50
    END ASSOCIATE

    ASSOCIATE (As =>  As%GetID())
      IF ( ANY(As .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) )) STOP 60
    END ASSOCIATE

    ASSOCIATE (As =>  As%Base%GetID())
      IF ( ANY(As .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) STOP 70
    END ASSOCIATE

    ASSOCIATE (As =>  As%Base%GetObj())
      IF ( ANY(As%BaseId       .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) STOP 71
      IF ( ANY(As%Base%GetID() .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) STOP 72
    END ASSOCIATE

    CALL As%SetId(As%Base, 1)
    CALL As%SetId(As, 2)

    IF ( ANY (As%GetID()      .NE. RESHAPE((/2,2,2,2/), (/2,2/)) ) ) STOP 83
    IF ( ANY (As%Base%GetID() .NE. RESHAPE((/1,1,1,1/), (/2,2/)) ) ) STOP 84

  END ASSOCIATE
  CLASS DEFAULT
    STOP 99
  END SELECT
  END ASSOCIATE

  END
