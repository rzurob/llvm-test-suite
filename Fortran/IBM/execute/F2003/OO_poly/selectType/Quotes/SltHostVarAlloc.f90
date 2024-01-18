! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 23, 2004
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
!*   The selector is an associate name associating to an allocatable
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


  PROGRAM SltHostVarAlloc
  USE M
  IMPLICIT  NONE
  CLASS(Zero), ALLOCATABLE :: Var

  ALLOCATE( Child :: Var)
  SELECT TYPE ( As => Var )
    CLASS DEFAULT
      SELECT TYPE ( Var )
        CLASS IS (Child)
          SELECT TYPE ( As )
            CLASS IS (Child)
              SELECT TYPE ( Var )
                CLASS DEFAULT
                  SELECT TYPE ( As )
                    CLASS DEFAULT
                      SELECT TYPE ( Var )
                        CLASS DEFAULT
                          IF ( As%Base%GetId() .NE.  1 ) STOP 34
                          IF ( As%GetId()      .NE.  2 ) STOP 35
                          IF ( As%BaseId       .NE.  1 ) STOP 36
                          IF ( As%ChildId      .NE.  2 ) STOP 37
                          CALL As%SetId()
                          CALL As%Base%SetId()
                          IF ( As%Base%GetId() .NE. -1 ) STOP 34
                          IF ( As%GetId()      .NE. -2 ) STOP 35
                          IF ( As%BaseId       .NE. -1 ) STOP 36
                          IF ( As%ChildId      .NE. -2 ) STOP 37
                      END SELECT
                  END SELECT
              END SELECT
          END SELECT
      END SELECT

    TYPE is (Base)
      STOP 32
    TYPE IS (Zero)
      STOP 38

  END SELECT


  END

