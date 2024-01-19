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
!*    The selector is an associate name associating to a nonpoly allocatable variable of derived types
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER, ALLOCATABLE :: BaseId
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER, ALLOCATABLE  :: ChildId
      TYPE(Base)            :: BaseComp
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

  PROGRAM HostAssocVarAlloc
  USE M
  IMPLICIT NONE

  TYPE(Child), ALLOCATABLE   :: U

  ALLOCATE(U)

  ASSOCIATE ( As => U )
  ASSOCIATE ( As => As )
    ALLOCATE(As%ChildId, SOURCE=2)
    ALLOCATE(As%BASE%BaseId, SOURCE=1)

    IF ( As%GetID() .NE. 2) ERROR STOP 50
    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( As0 .NE. 2) ERROR STOP 51
       IF ( As1 .NE. 1) ERROR STOP 52
    END ASSOCIATE
    ASSOCIATE ( As2 => As%Base )
      IF ( As2%GetID() .NE. 1 ) ERROR STOP 53
    END ASSOCIATE

    As%ChildId = -2
    As%BaseId  = -1
    IF (U%GetID()       .NE. -2 ) ERROR STOP 61
    IF (U%Base%GetID()  .NE. -1 ) ERROR STOP 62

  END ASSOCIATE
  END ASSOCIATE


  END
