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
!*    The selector is an associate name associating to a poly array variable of derived types
!*    (ICE)
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

  PROGRAM HostAssocArrPoly
  USE M
  IMPLICIT NONE

  CLASS(Base), ALLOCATABLE :: V(:)
  CLASS(*),    ALLOCATABLE :: U(:)

  ALLOCATE(V(3), SOURCE=Child(BaseId=-1, ChildId=-2))
  ALLOCATE(U(3), SOURCE=Child(BaseId=-1, ChildId=-2))

  ASSOCIATE ( T1 => V, T2 => U )
    SELECT TYPE ( T1 )
      TYPE IS ( Child )
        IF (ANY(T1%GetId() .NE. -2) )      ERROR STOP 30
        IF (ANY(T1%Base%GetId() .NE. -1) ) ERROR STOP 31
        T1%BaseId  = 1
        T1%ChildId = 2
      CLASS DEFAULT
        STOP 32
    END SELECT

    SELECT TYPE ( T2 )
      TYPE IS ( Child )
        IF (ANY(T2%GetId()      .NE. -2) ) ERROR STOP 40
        IF (ANY(T2%Base%GetId() .NE. -1) ) ERROR STOP 41
        T2%BaseId  = 1
        T2%ChildId = 2
      CLASS DEFAULT
        STOP 42
    END SELECT

    ASSOCIATE ( As1 => T1, As2 => T2 )
      SELECT TYPE ( As1 )
        TYPE IS (Child )
          IF (ANY(As1%GetId()      .NE. 2) ) ERROR STOP 50
          IF (ANY(As1%Base%GetId() .NE. 1) ) ERROR STOP 51
        CLASS DEFAULT
          STOP 52
      END SELECT

      SELECT TYPE ( As2 )
        TYPE IS (Child )
          IF (ANY(As2%GetId()      .NE. 2) ) ERROR STOP 60
          IF (ANY(As2%Base%GetId() .NE. 1) ) ERROR STOP 61
        CLASS DEFAULT
          STOP 62
      END SELECT
    END ASSOCIATE

  END ASSOCIATE


  DEALLOCATE(V)
  DEALLOCATE(U)

  END
