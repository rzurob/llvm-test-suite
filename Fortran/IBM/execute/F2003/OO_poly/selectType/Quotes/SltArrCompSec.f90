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
!*   The selector is a poly component array section
!*    (ICE-297844)
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

    TYPE :: Test
      CLASS(*), POINTER :: ChildArr(:,:,:)
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


  PROGRAM SltArrCompSec
  USE M
  IMPLICIT NONE
  TYPE(Child), TARGET :: Arr(2:19, 2:19, 2:19)
  TYPE(Test) :: V

  V%ChildArr => Arr

  SELECT TYPE ( V=>RESHAPE((/V%ChildArr(2:19:2, 2:19:2, 2:19:2)/), (/9,9,9/)) )
    CLASS DEFAULT
      SELECT TYPE (V=>V)
      CLASS IS (Zero)
      SELECT TYPE (V)
        TYPE IS (Child)
          IF ( ANY (LBOUND(V)     .NE. (/1, 1, 1/) ) )    ERROR STOP 30
          IF ( ANY (UBOUND(V)     .NE. (/9, 9, 9/) ) ) ERROR STOP 31
          IF ( ANY (SHAPE(V)      .NE. (/9, 9, 9/) ) ) ERROR STOP 32

          IF ( ANY(V%Base%GetId() .NE. 1) ) ERROR STOP 34
          IF ( ANY(V%GetId()      .NE. 2) ) ERROR STOP 35
          IF ( ANY(V%BaseId       .NE. 1) ) ERROR STOP 36
          IF ( ANY(V%ChildId      .NE. 2) ) ERROR STOP 37

          CALL V(2,2,2)%SetId(V)
          CALL V(2,2,2)%Base%SetId(V%Base)

          IF ( ANY(V%Base%GetId() .NE. -1 ) ) ERROR STOP 44
          IF ( ANY(V%GetId()      .NE. -2 ) ) ERROR STOP 45
          IF ( ANY(V%BaseId       .NE. -1 ) ) ERROR STOP 46
          IF ( ANY(V%ChildId      .NE. -2 ) ) ERROR STOP 47

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

  END

