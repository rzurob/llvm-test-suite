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
!*    The selector is a function returning an allocatable array
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


    FUNCTION Fun(Arg)
    CLASS(Base) :: Arg(2,*)
    CLASS(Base), ALLOCATABLE :: Fun(:,:)
      ALLOCATE(Fun(SIZE(Arg,1), 2), SOURCE=Arg(:, 1:2))
    END FUNCTION

  END MODULE

  PROGRAM ArrFuncAlloc
  USE M
  IMPLICIT NONE
  INTEGER :: i

  ASSOCIATE ( As => Fun((/Base(BaseId=-1), Base(BaseId=-2),Base(BaseId=-3), Base(BaseId=-4) /)) )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             ERROR STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             ERROR STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             ERROR STOP 32

    IF ( ANY (As%GetID()  .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) ) ) ERROR STOP 33
    IF ( ANY (As%BaseID   .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) ) ) ERROR STOP 34

    ASSOCIATE ( As1 => As%BaseId )
       IF ( ANY(As1 .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) ) ) ERROR STOP 42
    END ASSOCIATE

    ASSOCIATE (As =>  As%GetID())
      IF ( ANY(As .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) )) ERROR STOP 60
    END ASSOCIATE

    SELECT TYPE (As => As)
    CLASS IS ( Child)
      STOP 70
    CLASS DEFAULT
      STOP 80
    TYPE IS (Base)
    END SELECT

  END ASSOCIATE

  END
