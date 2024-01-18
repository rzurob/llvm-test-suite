! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 14, 2004
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
!*   The selector is specified with a poly abstract component
!*    (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      CLASS(Base), POINTER :: BasePtr(:)=>NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Child) :: Abs
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

  PROGRAM SltAbsCom1
  USE M
  IMPLICIT NONE

  CLASS(Base) ,ALLOCATABLE :: Var
  TYPE(Child), TARGET :: Tar(4)

  ALLOCATE(Var, SOURCE=Child())

  SELECT TYPE (Var)
     TYPE IS (Child)

      Var%BasePtr => Tar
      SELECT TYPE ( As => Var%BasePtr )
        CLASS DEFAULT
          STOP 40
        TYPE IS (Child)
          IF ( ANY(SHAPE(As) .NE. (/4/))  )   STOP 41
          IF ( LBOUND(As, 1) .NE. 1       )   STOP 42
          IF ( ANY(As%ChildId      .NE. (/2,2,2,2/)) )  STOP 43
          IF ( ANY(As%GetId()      .NE. (/2,2,2,2/)) )  STOP 43
          IF ( ANY(As%Base%BaseId  .NE. (/1,1,1,1/)) )  STOP 44
!         IF ( ANY(As%Base%GetId() .NE. (/1,1,1,1/)) )  STOP 44 !C611
      END SELECT

    CLASS DEFAULT
      STOP 20
  END SELECT

  END

