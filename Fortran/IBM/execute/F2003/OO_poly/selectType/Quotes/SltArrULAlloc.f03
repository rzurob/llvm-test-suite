! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 06, 2005
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
!*   The selector is an array of allocatable
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
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
    CLASS(Base) :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base)  :: Arg(:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrULAlloc
  USE M
  IMPLICIT NONE

  CLASS(*), ALLOCATABLE :: Arr(:,:)

  ALLOCATE(Arr(20, 20), SOURCE=Child(ChildId=-2, BaseId=-1))

  SELECT TYPE ( As => Arr(:, 2:19))
    CLASS IS (Child)
      SELECT TYPE (As => As(1::2, :))
        TYPE IS (Child)

          IF ( ANY (LBOUND(As)     .NE. (/1,1/) ) )  ERROR STOP 30
          IF ( ANY (UBOUND(As)     .NE. (/10,18/) ) ) ERROR STOP 31
          IF ( ANY (SHAPE(As)      .NE. (/10,18/) ) ) ERROR STOP 32

          IF ( ANY(As%Base%GetId() .NE. -1) ) ERROR STOP 34

          IF ( ANY(As%Base%GetId() .NE. -1) ) ERROR STOP 34
          IF ( ANY(As%GetId()      .NE. -2) ) ERROR STOP 35
          IF ( ANY(As%BaseId       .NE. -1) ) ERROR STOP 36
          IF ( ANY(As%ChildId      .NE. -2) ) ERROR STOP 37

          CALL As(1,1)%SetId(As)
          CALL As(1,1)%Base%SetId(As%Base)

          IF ( ANY(As%Base%GetId() .NE. 1 ) ) ERROR STOP 44
          IF ( ANY(As%GetId()      .NE. 2 ) ) ERROR STOP 45
          IF ( ANY(As%BaseId       .NE. 1 ) ) ERROR STOP 46
          IF ( ANY(As%ChildId      .NE. 2 ) ) ERROR STOP 47

       CLASS DEFAULT
          STOP 40
      END SELECT

    TYPE is (Base)
      STOP 32
    TYPE IS (Zero)
      STOP 38

  END SELECT

  END
