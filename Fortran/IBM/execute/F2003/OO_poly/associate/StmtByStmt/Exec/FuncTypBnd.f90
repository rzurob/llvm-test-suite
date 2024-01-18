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
!*    (Incorrect result :stop 60)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: ReturnBase
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: ReturnChild
    END TYPE

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    FUNCTION ReturnBase(Arg)
    CLASS(Base)  :: Arg
    CLASS(Base), ALLOCATABLE  :: ReturnBase
      Arg%BaseId = -1
      SELECT TYPE (Arg)
        TYPE IS (Child)
          Arg%ChildId = -2
      END SELECT
      ALLOCATE(ReturnBase, SOURCE=Arg)
    END FUNCTION

    FUNCTION ReturnChild(Arg)
    CLASS(Child)       :: Arg
    CLASS(*), POINTER  :: ReturnChild
      Arg%BaseId  = -1
      Arg%ChildId = -2
      ALLOCATE(ReturnChild, SOURCE=Arg)
    END FUNCTION

  END MODULE

  PROGRAM FuncTypBnd
  USE M
  TYPE(Child) :: V

  ASSOCIATE ( As => V )
  ASSOCIATE ( As1 => As%ReturnBase() )
  ASSOCIATE ( As2 => As%Base%ReturnBase() )
  ASSOCIATE ( As3 => As%ReturnChild() )

    As%BaseId  = 1
    As%ChildId = 2
    IF ( As1%GetID()          .NE. -2) STOP 51
    SELECT TYPE (As1)
      TYPE IS (Child)
        IF ( As1%Base%GetID() .NE. -1) STOP 50
      CLASS DEFAULT
        STOP 52
    END SELECT

    As%BaseId  = 1
    As%ChildId = 2
    IF ( As2%GetID() .NE. -1) STOP 60

    As%BaseId  = 1
    As%ChildId = 2
    SELECT TYPE (As3)
      TYPE IS (Child)
        IF ( As3%GetID()      .NE. -2) STOP 70
        IF ( As3%Base%GetID() .NE. -1) STOP 80
      CLASS DEFAULT
        STOP 90
    END SELECT

  END ASSOCIATE
  END ASSOCIATE
  END ASSOCIATE
  END ASSOCIATE

  END
