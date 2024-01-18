! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 19, 2005
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
!*   The selector is an allocatable  array from function call
!*   forming a array section
!*    (ICE)
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
    CLASS(Base), INTENT(INOUT) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base), INTENT(INOUT)  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrFuncPolyAllocSec
  USE M
  IMPLICIT NONE
  TYPE(Child), TARGET :: V(4,4)

  V%BaseId = -1
  V%ChildId = -2

  CALL Sub(V(1:3,::2))

  CONTAINS

  FUNCTION Fun(Arg)
  CLASS(*) :: Arg(:,:)
  CLASS(*), ALLOCATABLE :: Fun(:,:)
    ALLOCATE(Fun(SIZE(Arg,1),SIZE(Arg,2)), SOURCE=Arg)
  END FUNCTION

  SUBROUTINE Sub(Arg)
  CLASS(Base) :: Arg(:, :)

  SELECT TYPE ( V => Fun(Arg(1::2,::1)) )
    CLASS DEFAULT
      SELECT TYPE (V => V(:,:))
        TYPE IS (Child)

          IF ( SIZE(V)          .NE. 4 )          ERROR STOP 21
          IF ( ANY (LBOUND(V)   .NE. (/1, 1/) ) ) ERROR STOP 30
          IF ( ANY (UBOUND(V)   .NE. (/2, 2/) ) ) ERROR STOP 31
          IF ( ANY(SHAPE(V)     .NE. (/2,2/)) )   ERROR STOP 20

          IF ( ANY(V%Base%GetId() .NE. -1) ) ERROR STOP 34
          IF ( ANY(V%GetId()      .NE. -2) ) ERROR STOP 35
          IF ( ANY(V%BaseId       .NE. -1) ) ERROR STOP 36
          IF ( ANY(V%ChildId      .NE. -2) ) ERROR STOP 37

          IF ( .NOT. V%Called() ) ERROR STOP 45

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

  END SUBROUTINE


  END



