! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 14, 2005
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
!*    The selector is a host associate name
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
      private
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
    CLASS(Base) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base)  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM ArrHostPolyPtr
  USE M
  IMPLICIT NONE
  TYPE(Child), TARGET :: Tar

  CLASS(Child), POINTER :: Arr(:)

  ALLOCATE(Arr(3), SOURCE=Child(ChildId=-2, BaseId=-1))

  CALL Sub(Arr)

  CONTAINS

  FUNCTION Fun(Arr1, Arr2)
  CLASS(Child), POINTER :: Fun(:)
  CLASS(Child) :: Arr1(:), Arr2(:)
    ALLOCATE(Fun(SIZE(Arr)), SOURCE = Arr1)
    SELECT TYPE (Arr2)
    TYPE IS (Child)
      Arr2 =Arr1
    END SELECT
  END FUNCTION

  SUBROUTINE Sub(Arr)
  CLASS(Child), POINTER :: Arr(:)
  TYPE (CHILD) :: V(SIZE(Arr))

  ASSOCIATE ( As => V )
  ASSOCIATE ( As => Fun(Arr, As))
    SELECT TYPE ( As )
    CLASS IS (Child)

          IF (ANY(SHAPE(As) .NE. (/3/)))      STOP 33
          IF ( ANY(As%Base%GetId() .NE. -1) ) STOP 34
          IF ( ANY(As%GetId()      .NE. -2) ) STOP 35
          IF ( ANY(As%BaseId       .NE. -1) ) STOP 36
          IF ( ANY(As%ChildId      .NE. -2) ) STOP 37

          CALL As(1)%SetId(As)
          CALL As(1)%Base%SetId(As%Base)

          IF ( ANY(As%Base%GetId() .NE. 1 ) ) STOP 44
          IF ( ANY(As%GetId()      .NE. 2 ) ) STOP 45
          IF ( ANY(As%BaseId       .NE. 1 ) ) STOP 46
          IF ( ANY(As%ChildId      .NE. 2 ) ) STOP 47


    CLASS DEFAULT
      STOP 38

  END SELECT

  END ASSOCIATE

  IF (ANY(SHAPE(As) .NE. (/3/)))      STOP 53
  IF ( ANY(As%Base%GetId() .NE. -1) ) STOP 54
  IF ( ANY(As%GetId()      .NE. -2) ) STOP 55
  IF ( ANY(As%BaseId       .NE. -1) ) STOP 56
  IF ( ANY(As%ChildId      .NE. -2) ) STOP 57

  END ASSOCIATE

  END SUBROUTINE

  END



