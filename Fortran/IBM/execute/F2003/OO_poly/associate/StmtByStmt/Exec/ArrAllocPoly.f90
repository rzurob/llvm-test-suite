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
!*    The selector is an array of poly allocatable
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

  PROGRAM ArrAllocPoly
  USE M
  IMPLICIT NONE

  CLASS(Child), ALLOCATABLE :: Alloc(:,:)

  ALLOCATE(Child :: Alloc(2,2))

  ASSOCIATE ( MainAs => Alloc )

    CALL Sub(Alloc)

    SELECT TYPE (MainAs)
    TYPE IS (Child)
      ASSOCIATE ( As => MainAs%ChildId, As1 => MainAs%BaseId )
         IF ( ANY(As .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 41
         IF ( ANY(As1 .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 42
      END ASSOCIATE

      ASSOCIATE ( As2 => MainAs%Base )
        IF ( ANY(As2%GetID() .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) STOP 50
      END ASSOCIATE
    CLASS DEFAULT
      STOP 51
    END SELECT

  END ASSOCIATE

  DEALLOCATE(Alloc)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(Child) :: Arg(:,:)

  ASSOCIATE ( As => Arg )

    Arg%ChildId = -2
    Arg%Base%BaseId = -1

    IF ( ANY (LBOUND(As)      .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (SHAPE(As)       .NE. (/2,2/) ) )             STOP 32
    IF ( ANY (As%GetID()      .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 33
    IF ( ANY (As%Base%GetID() .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 34

    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( ANY(As0 .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 41
       IF ( ANY(As1 .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 42
    END ASSOCIATE

    ASSOCIATE ( As2 => As%Base )
      IF ( ANY(As2%GetID() .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) STOP 50
    END ASSOCIATE

  END ASSOCIATE

  ASSOCIATE (As =>  Arg%GetID())
    IF ( ANY(As .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) )) STOP 60
  END ASSOCIATE

  ASSOCIATE (As =>  Arg%Base%GetID())
    IF ( ANY(As .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) STOP 70
  END ASSOCIATE

  END SUBROUTINE

  END
