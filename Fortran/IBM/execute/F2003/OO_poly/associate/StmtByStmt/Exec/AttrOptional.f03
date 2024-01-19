! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 22, 2005
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
!*
!*   The selector has the optional attribute
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
    CLASS(Base) :: Arg
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base)  :: Arg
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE


  END MODULE


  PROGRAM AttrOptional
  USE M
  INTEGER :: i
  TYPE (Child) :: W=Child(BaseID=-1, ChildID=-2)

  CALL Sub(W)

  ! ID changed

  IF ( W%BaseID        .NE. 1 ) ERROR STOP 40
  IF ( W%Base%GetId()  .NE. 1 ) ERROR STOP 41
  IF ( W%ChildID       .NE. 2 ) ERROR STOP 42
  IF ( W%GetId()       .NE. 2 ) ERROR STOP 43

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*), OPTIONAL :: Arg

  IF ( .NOT. PRESENT (Arg) ) ERROR STOP 11

  ASSOCIATE ( Arg => Arg )
  SELECT TYPE ( Arg )
  CLASS IS (Child)

    IF ( Arg%BaseID        .NE. -1 ) ERROR STOP 30
    IF ( Arg%Base%GetId()  .NE. -1 ) ERROR STOP 31
    IF ( Arg%ChildID       .NE. -2 ) ERROR STOP 32
    IF ( Arg%GetId()       .NE. -2 ) ERROR STOP 33

    CALL Arg%Base%SetID(Arg)
    CALL Arg%SetID(Arg)

  CLASS DEFAULT
    STOP 99
  END SELECT

  END ASSOCIATE

  END SUBROUTINE

  END
