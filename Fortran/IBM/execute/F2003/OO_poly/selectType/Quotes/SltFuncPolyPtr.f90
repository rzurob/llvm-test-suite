! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 16, 2004
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
!*   The selector is a poly func call returning a pointer entity.
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
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      CLASS(Base), POINTER :: BasePtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
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
    CLASS(Base)  :: Arg
      Arg%BaseId = -1
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child)  :: Arg
      Arg%ChildId = -2
    END SUBROUTINE

  END MODULE


  PROGRAM SltFuncPolyPtr
  USE M
  IMPLICIT  NONE

  INTERFACE
    FUNCTION Func(Arg)
      CLASS(*) :: Arg
      CLASS(*), POINTER :: Func
    END FUNCTION
  END INTERFACE


  SELECT TYPE ( As => Func(Zero()))
    CLASS IS (Zero)
      STOP 20
    TYPE is (CHARACTER(*))
      STOP 21
    TYPE is (Base)
      IF ( As%GetId() .NE. 1 ) STOP 22
      IF ( As%BaseId  .NE. 1 ) STOP 23
    CLASS DEFAULT
      STOP 24
  END SELECT

  SELECT TYPE ( As => Func(Func(Func(Zero()))))
    CLASS DEFAULT
      STOP 30
    TYPE is (CHARACTER(*))
      STOP 31
    TYPE is (Base)
      STOP 32
    TYPE is (REAL)
      STOP 33
    CLASS IS (Child)
      IF ( As%Base%GetId() .NE. 1 ) STOP 34
      IF ( As%GetId()      .NE. 2 ) STOP 35
      IF ( As%BaseId       .NE. 1 ) STOP 36
      IF ( As%ChildId      .NE. 2 ) STOP 37
    CLASS IS (Zero)
      STOP 38
  END SELECT

  SELECT TYPE ( As => Func(123))
    CLASS DEFAULT
      STOP 40
    TYPE is (CHARACTER(*))
      IF (As .NE. "UNKNOWN!" ) STOP  41
    TYPE is (Base)
      STOP 42
    CLASS IS (Base)
      STOP 43
  END SELECT



  END

  FUNCTION Func(Arg)
  USE M
  CLASS(*) :: Arg
  CLASS(*), POINTER :: Func

  SELECT TYPE (Arg)
    CLASS IS (Zero)
      ALLOCATE(Func, SOURCE=Base())
    CLASS IS (Base)
      ALLOCATE(Func, SOURCE=Child(Base=Arg))
    CLASS IS (Child)
      ALLOCATE(Func, SOURCE=Arg)
    CLASS DEFAULT
      ALLOCATE(Func, SOURCE="UNKNOWN!")
  END SELECT
  END FUNCTION
