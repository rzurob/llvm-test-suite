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
!*    The selector a non poly constant array
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

  PROGRAM ArrConst
  USE M
  IMPLICIT NONE

  TYPE(Child), PARAMETER  :: U(2,2) = Child()

  ASSOCIATE ( As => U )
    IF ( ANY (LBOUND(As)      .NE. (/1,1/) ) )             ERROR STOP 30
    IF ( ANY (SHAPE(As)       .NE. (/2,2/) ) )             ERROR STOP 32
    IF ( ANY (As%GetID()      .NE. RESHAPE((/2,2,2,2/), (/2,2/)) ) ) ERROR STOP 33
    IF ( ANY (As%Base%GetID() .NE. RESHAPE((/1,1,1,1/), (/2,2/)) ) ) ERROR STOP 34

    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( ANY(As0 .NE. RESHAPE((/2,2,2,2/), (/2,2/)) ) ) ERROR STOP 41
       IF ( ANY(As1 .NE. RESHAPE((/1,1,1,1/), (/2,2/)) ) ) ERROR STOP 42
    END ASSOCIATE

    ASSOCIATE ( As2 => As%Base )
      IF ( ANY(As2%GetID() .NE. RESHAPE((/1,1,1,1/), (/2,2/)) )) ERROR STOP 50
    END ASSOCIATE

  END ASSOCIATE

  ASSOCIATE (As =>  U%GetID())
    IF ( ANY(As .NE. RESHAPE((/2,2,2,2/), (/2,2/)) )) ERROR STOP 60
  END ASSOCIATE

  ASSOCIATE (As =>  U%Base%GetID())
    IF ( ANY(As .NE. RESHAPE((/1,1,1,1/), (/2,2/)) )) ERROR STOP 70
  END ASSOCIATE

  END
