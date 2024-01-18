! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  HostAssocVarPolyAlloc.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : HostAssocVarPolyAlloc
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
!*    The selector is an associate name associating to a poly allocatable variable of derived types
!*    (Wrong result: stop 31)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER    :: ChildId = 2
      TYPE(Base) :: BaseComp
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
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

  END MODULE

  PROGRAM HostAssocVarPolyAlloc
  USE M
  IMPLICIT NONE

  CLASS(Base), ALLOCATABLE   :: U

  ALLOCATE(U, SOURCE=Child(BaseId=-1, ChildId=-2, BaseComp=Base(0)))

  ASSOCIATE ( As => U )
  ASSOCIATE ( As => As )

    SELECT TYPE ( As )
      TYPE IS ( Child )
        IF ( As%GetId()      .NE. -2 ) STOP 30
        IF ( As%ChildId      .NE. -2 ) STOP 31
        IF ( As%Base%GetId() .NE. -1 ) STOP 32
        IF ( As%BaseComp%GetId() .NE. 0 ) STOP 33

        As%BaseId  = 1
        As%ChildId = 2

        IF ( As%GetId()      .NE. 2 ) STOP 40
        IF ( As%ChildId      .NE. 2 ) STOP 41
        IF ( As%Base%GetId() .NE. 1 ) STOP 42
        IF ( U%GetId()       .NE. 2 ) STOP 43
      CLASS DEFAULT
        STOP 50
    END SELECT
  END ASSOCIATE
  END ASSOCIATE

  END
