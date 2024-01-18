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
!*    The selector is an associate name associating to structure constructor and its component
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
      INTEGER, ALLOCATABLE :: IntArr(:)
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

  PROGRAM HostAssocStruct
  USE M
  IMPLICIT NONE

  ASSOCIATE (T =>  Child(BaseId=-1, ChildId=-2, IntArr=(/-1,-2,-3/)))
  ASSOCIATE (As0  => T, As1 => T%Base, As2 => T%IntArr )
    IF ( As0%GetId()      .NE. -2 )            STOP 30
    IF ( As0%Base%GetId() .NE. -1 )            STOP 31
    IF ( ANY(As0%IntArr   .NE. (/-1,-2,-3/)) ) STOP 32

    IF ( As1%GetId() .NE. -1 ) STOP  40

    IF ( ANY(As2 .NE. (/-1,-2,-3/)) ) STOP 50

  END ASSOCIATE
  END ASSOCIATE


  END
