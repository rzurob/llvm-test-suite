! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 20, 2005
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
!*   The selector is a host associate name associating to a
!*   poly assumed size dummy array
!*    (wrong associating entity's shape-298522)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
    CONTAINS
      PROCEDURE, NoPASS   :: Called
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

    FUNCTION Called()
    LOGICAL :: Called
      Called =.true.
    END FUNCTION

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
    CLASS(Base), INTENT(INOUT) :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base), INTENT(INOUT)  :: Arg(:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrHostDummyAssumSiz
  USE M
  IMPLICIT NONE
  TYPE(Child), TARGET :: V(4)

  V%BaseId = -1
  V%ChildId = -2

  CALL Sub(V)
  ASSOCIATE (W=>V)
    IF ( ANY(W%Base%GetId() .NE. 1) ) ERROR STOP 34
    IF ( ANY(W%GetId()      .NE. 2) ) ERROR STOP 35
    IF ( ANY(W%BaseId       .NE. 1) ) ERROR STOP 36
    IF ( ANY(W%ChildId      .NE. 2) ) ERROR STOP 37
  END ASSOCIATE

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(Base), TARGET, OPTIONAL :: Arg(2, *)

    IF ( .NOT. PRESENT(Arg) ) ERROR STOP 11

    SELECT TYPE (U => Arg(:,:2))
    CLASS IS (Child)
    SELECT TYPE (W => U)
    CLASS IS (Child)
      SELECT TYPE (V => W)
        TYPE IS (Child)
          IF ( SIZE(V)          .NE. 4 )          ERROR STOP 21
          IF ( ANY (LBOUND(V)   .NE. (/1, 1/) ) ) ERROR STOP 30
          IF ( ANY (UBOUND(V)   .NE. (/2, 2/) ) ) ERROR STOP 31
          IF ( ANY(SHAPE(V)     .NE. (/2,2/)) )   ERROR STOP 20

          IF ( ANY(W%Base%GetId() .NE. -1) ) ERROR STOP 34
          IF ( ANY(W%GetId()      .NE. -2) ) ERROR STOP 35
          IF ( ANY(W%BaseId       .NE. -1) ) ERROR STOP 36
          IF ( ANY(W%ChildId      .NE. -2) ) ERROR STOP 37

          IF ( .NOT. V%Called() ) ERROR STOP 45

          CALL V%SetId(U)
          CALL W%Base%SetId(V%Base)

          IF ( ANY (U%Base%GetId() .NE. 1 )) ERROR STOP 44
          IF ( ANY (U%GetId()      .NE. 2 )) ERROR STOP 45
          IF ( ANY (U%BaseId       .NE. 1 )) ERROR STOP 46
          IF ( ANY (U%ChildId      .NE. 2 )) ERROR STOP 47

       CLASS DEFAULT
          STOP 40
        CLASS is (Child)
          STOP 56
      END SELECT

  END SELECT
  END SELECT

  END SUBROUTINE


  END



