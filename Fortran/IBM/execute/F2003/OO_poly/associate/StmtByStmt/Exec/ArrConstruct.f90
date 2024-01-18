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
!*    The selector an array construct
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

  PROGRAM ArrConstruct
  USE M
  IMPLICIT NONE
  integer i

  ASSOCIATE ( As => (/Child(BaseId=0, ChildId=0),(Child(BaseId=i, ChildId=-i), i=1, 6)/) )
    IF ( ANY (LBOUND(As)      .NE. (/1/) ) )             ERROR STOP 30
    IF ( ANY (SHAPE(As)       .NE. (/7/) ) )             ERROR STOP 32
    IF ( ANY (As%GetID()      .NE. (/0,-1,-2,-3,-4,-5,-6/)) ) ERROR STOP 33
    IF ( ANY (As%Base%GetID() .NE. (/0, 1, 2, 3, 4, 5, 6/)) ) ERROR STOP 34

    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( ANY(As0 .NE. (/0,-1,-2,-3,-4,-5,-6/) ) ) ERROR STOP 41
       IF ( ANY(As1 .NE. (/0, 1, 2, 3, 4, 5, 6/) ) ) ERROR STOP 42
    END ASSOCIATE

    ASSOCIATE ( As2 => As%Base )
      IF ( ANY(As2%GetID() .NE. (/0, 1, 2, 3, 4, 5, 6/) )) ERROR STOP 50
    END ASSOCIATE

    ASSOCIATE (As1 =>  As%GetID())
      IF ( ANY(As1 .NE. (/0,-1,-2,-3,-4,-5,-6/) )) ERROR STOP 60
    END ASSOCIATE

    ASSOCIATE (As1 => As%Base%GetID())
      IF ( ANY(As1 .NE.  (/0, 1, 2, 3, 4, 5, 6/) )) ERROR STOP 70
    END ASSOCIATE
  END ASSOCIATE

  END
