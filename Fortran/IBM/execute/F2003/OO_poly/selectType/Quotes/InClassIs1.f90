! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 21, 2005
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
!*  Within the CLASS IS, the associating entity is polymorphic
!*
!*    ()
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


  PROGRAM InClassIs1
  USE M
  IMPLICIT NONE
  TYPE(Child) :: V(2:3,3:4)=Child(BaseId=-1, ChildId=-2)

  CALL Sub(V(2:3,3:4))

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(2:3,3:4)

    SELECT TYPE (U => Arg)
    CLASS IS (Base)
       STOP 43
    CLASS IS (Child)
      SELECT TYPE (U)
      CLASS IS (Child)
        IF ( .NOT. SAME_TYPE_AS(U, Arg))       ERROR STOP 30
        IF ( SIZE(U)          .NE. 4 )          ERROR STOP 31
        IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) ERROR STOP 32
        IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) ERROR STOP 33
        IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   ERROR STOP 34
        IF ( ANY(U%Base%GetId() .NE. -1) )      ERROR STOP 35
        IF ( ANY(U%GetId()      .NE. -2) )      ERROR STOP 36
        IF ( ANY(U%BaseId       .NE. -1) )      ERROR STOP 37
        IF ( ANY(U%ChildId      .NE. -2) )      ERROR STOP 38

        IF ( .NOT. U%Called() ) ERROR STOP 45
      CLASS DEFAULT
         STOP 51
      END SELECT

    TYPE IS (Base)
       STOP 40
    CLASS DEFAULT
       STOP 41
    END SELECT

  END SUBROUTINE

  END



