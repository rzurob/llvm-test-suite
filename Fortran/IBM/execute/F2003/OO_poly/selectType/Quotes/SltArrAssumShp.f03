! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 07, 2005
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
!*   The selector is an assumed shape  array
!*    (ICE-297853)
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
    CLASS(Base) :: Arg(:,:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base)  :: Arg(:,:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrAssumShp
  USE M
  IMPLICIT NONE
  TYPE(Child) :: Arr(18, 18,18)

  CALL Sub(Arr)

  CONTAINS

  SUBROUTINE Sub(Arr)
  CLASS(*) :: Arr(2:, 2:, 2:)

  SELECT TYPE ( Arr )
    CLASS DEFAULT
      SELECT TYPE (Ptr=>Arr)
      CLASS IS (Zero)
      SELECT TYPE (Ptr)
        TYPE IS (Child)
          IF ( ANY (LBOUND(Ptr)     .NE. (/2, 2, 2/) ) )    ERROR STOP 30
          IF ( ANY (UBOUND(Ptr)     .NE. (/19, 19, 19/) ) ) ERROR STOP 31
          IF ( ANY (SHAPE(Ptr)      .NE. (/18, 18, 18/) ) ) ERROR STOP 32

          IF ( ANY(Ptr%Base%GetId() .NE. 1) ) ERROR STOP 34
          IF ( ANY(Ptr%GetId()      .NE. 2) ) ERROR STOP 35
          IF ( ANY(Ptr%BaseId       .NE. 1) ) ERROR STOP 36
          IF ( ANY(Ptr%ChildId      .NE. 2) ) ERROR STOP 37

          CALL Ptr(2,2,2)%SetId(Ptr)
          CALL Ptr(2,2,2)%Base%SetId(Ptr%Base)

          IF ( ANY(Ptr%Base%GetId() .NE. -1 ) ) ERROR STOP 44
          IF ( ANY(Ptr%GetId()      .NE. -2 ) ) ERROR STOP 45
          IF ( ANY(Ptr%BaseId       .NE. -1 ) ) ERROR STOP 46
          IF ( ANY(Ptr%ChildId      .NE. -2 ) ) ERROR STOP 47

       CLASS DEFAULT
          STOP 40
        CLASS is (Child)
          STOP 56
        TYPE is (Base)
          STOP 57
        TYPE IS (Zero)
          STOP 58
      END SELECT
      END SELECT

  END SELECT

  END SUBROUTINE

  END

