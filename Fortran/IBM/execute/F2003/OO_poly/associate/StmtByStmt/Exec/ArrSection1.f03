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
!*    The selector is an array  formed from component of array section
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero
    END TYPE

    TYPE, EXTENDS(Zero) :: Base
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

  PROGRAM ArrSection1
  USE M
  IMPLICIT NONE
  INTEGER :: i

  TYPE(Child) :: Arr(4, 4)

  Arr = RESHAPE((/(Child(BaseId=i, ChildId=-i), i=1, 16) /), (/4,4/))

  ASSOCIATE ( As => Arr(1:4:2, 2:4:2)%Base)
    IF ( ANY (LBOUND(As)      .NE. (/1,1/) ) )             ERROR STOP 30
    IF ( ANY (SHAPE(As)       .NE. (/2,2/) ) )             ERROR STOP 32
    IF ( ANY (As%GetID() .NE. RESHAPE((/ 5, 7, 13, 15/), (/2,2/)) ) ) ERROR STOP 34
  END ASSOCIATE

  ASSOCIATE ( As => Arr(1:4:2, 2:4:2)%ChildId)
    IF ( ANY (LBOUND(As)   .NE. (/1,1/) ) )             ERROR STOP 40
    IF ( ANY (SHAPE(As)    .NE. (/2,2/) ) )             ERROR STOP 42
    IF ( ANY (As           .NE. RESHAPE((/ -5, -7, -13, -15/), (/2,2/)) ) ) ERROR STOP 44
  END ASSOCIATE

  ASSOCIATE ( As => Arr(1:4:2, 2:4:2)%Base%GetId())
    IF ( ANY (LBOUND(As)   .NE. (/1,1/) ) )             ERROR STOP 30
    IF ( ANY (SHAPE(As)    .NE. (/2,2/) ) )             ERROR STOP 32
    IF ( ANY (As           .NE. RESHAPE((/ 5, 7, 13, 15/), (/2,2/)) ) ) ERROR STOP 34
  END ASSOCIATE


  END