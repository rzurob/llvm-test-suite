! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 15, 2004
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
!*   The selector is a var of derived type. This's a very basic usage
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
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

  END MODULE

  PROGRAM SltDerVar
  USE M
  IMPLICIT NONE

  CLASS(*), ALLOCATABLE :: Var

  ALLOCATE(Var, SOURCE=Child(BaseId=-1,ChildId=-2))

  SELECT TYPE (Var)
    CLASS DEFAULT
      STOP 20
    TYPE IS (INTEGER(4))
      STOP 21
    TYPE IS (Base)
      STOP 22
    CLASS IS (Base)
      STOP 23
    CLASS is (Child)
      STOP 24
    TYPE IS (Child)
      IF ( Var%BaseId       .NE. -1 ) STOP 31
      IF ( Var%Base%BaseId  .NE. -1 ) STOP 32
      IF ( Var%ChildId      .NE. -2 ) STOP 33
      IF ( Var%Base%GetId() .NE. -1 ) STOP 34
      IF ( Var%GetId()      .NE. -2 ) STOP 35
  END SELECT


  END
