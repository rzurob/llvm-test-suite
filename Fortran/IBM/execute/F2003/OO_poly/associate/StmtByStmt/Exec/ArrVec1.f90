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
!*    The selector is an unlimited poly array section with a vector subscript
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

  PROGRAM ArrVec1
  USE M
  IMPLICIT NONE

  INTEGER :: i
  CLASS(*), POINTER :: Arr(:)

  ALLOCATE( Arr(10), SOURCE=(/(Child(BaseId=i, ChildId=-i), i=1,10)/) )

  ASSOCIATE (  T => Arr(2::2))
  ASSOCIATE ( As => T((/4,1,3,2/)) )
  SELECT TYPE ( As )
  TYPE IS (Child)

    IF ( ANY (LBOUND(As)      .NE. (/1/) ) )             STOP 30
    IF ( ANY (SHAPE(As)       .NE. (/4/) ) )             STOP 32
    IF ( ANY (As%GetID()      .NE. (/-8,-2,-6,-4/) ) ) STOP 33
    IF ( ANY (As%Base%GetID() .NE. (/ 8, 2, 6, 4/) ) ) STOP 34

    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( ANY(As0 .NE. (/-8,-2,-6,-4/) ) ) STOP 41
       IF ( ANY(As1 .NE. (/ 8, 2, 6, 4/) ) ) STOP 42
    END ASSOCIATE

    ASSOCIATE ( As2 => As%Base )
      IF ( ANY(As2%GetID() .NE. (/ 8, 2, 6, 4/) )) STOP 50
    END ASSOCIATE

    ASSOCIATE (As1 =>  As%GetID())
      IF ( ANY(As1 .NE. (/-8,-2,-6,-4/) )) STOP 60
    END ASSOCIATE

    ASSOCIATE (As1 =>  As%Base%GetID())
      IF ( ANY(As1 .NE. (/ 8, 2, 6, 4/) )) STOP 70
    END ASSOCIATE

  CLASS DEFAULT
    STOP 99
  END SELECT
  END ASSOCIATE
  END ASSOCIATE

  END
