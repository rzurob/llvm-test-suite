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
!*   The selector is a poly expr with defined operator
!*    (297388)
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

    INTERFACE OPERATOR ( .OP. )
      FUNCTION MyAdd (Arg1, Arg2)
        IMPORT Base, Child
        CLASS(Base),  INTENT(IN) :: Arg1
        CLASS(*),     INTENT(IN) :: Arg2
        CLASS(*),     POINTER    :: MyAdd
      END FUNCTION
    END INTERFACE OPERATOR ( .OP. )

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


  PROGRAM SltDefOp
  USE M
  IMPLICIT  NONE

  Call Sub(Child(), Child())

  CONTAINS

  SUBROUTINE Sub(Arg1, Arg2)
  CLASS(Base) :: Arg1
  CLASS(Zero) :: Arg2

  SELECT TYPE ( As=> Arg1 .OP. Arg2 )
    CLASS DEFAULT
      STOP 30
    TYPE is (Base)
      STOP 32
    CLASS IS (Child)
      IF ( As%Base%GetId() .NE.  2 ) ERROR STOP 34
      IF ( As%GetId()      .NE.  4 ) ERROR STOP 35
      IF ( As%BaseId       .NE.  2 ) ERROR STOP 36
      IF ( As%ChildId      .NE.  4 ) ERROR STOP 37
    CLASS IS (Zero)
      STOP 38
  END SELECT
  END SUBROUTINE

  END

  FUNCTION MyAdd (Arg1, Arg2)
  USE M, ONLY: Base, Child
    CLASS(Base),  INTENT(IN) :: Arg1
    CLASS(*),     INTENT(IN) :: Arg2
    CLASS(*),     POINTER    :: MyAdd

    ALLOCATE(Child::MyAdd)

    SELECT TYPE (MyAdd)
      TYPE IS (Child)
        SELECT TYPE (Arg1)
          TYPE IS (Child)
            SELECT TYPE (Arg2)
              TYPE IS (Child)
                MyAdd%BaseId  = Arg1%BaseId  + Arg2%BaseId
                MyAdd%ChildId = Arg1%ChildId + Arg2%ChildId
            END SELECT
        END SELECT
    END SELECT
  END FUNCTION

