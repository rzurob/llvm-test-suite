! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 22, 2004
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
!*    The selector is a poly dummy
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

  PROGRAM ArrDummyPoly
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr(:,:)
  CLASS(Base), POINTER  :: Tar(:,:)

  ALLOCATE(Child :: Tar(2,2))
  Ptr => Tar

  ASSOCIATE ( As => Ptr )
    SELECT TYPE (As)
      TYPE IS (Child)
        CALL Sub(Ptr)
        IF ( ANY(As%ChildId .NE. -2) ) ERROR STOP 21
        IF ( ANY(As%BaseId  .NE. -1) ) ERROR STOP 22
        IF ( ANY(As%GetID()      .NE. -2) ) ERROR STOP 23
        IF ( ANY(As%Base%GetID() .NE. -1) ) ERROR STOP 24
      CLASS DEFAULT
      STOP 25
    END SELECT

  END ASSOCIATE

  DEALLOCATE(Ptr)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*), POINTER :: Arg(:,:)

  ASSOCIATE ( As => Arg )
    SELECT TYPE (As)
      TYPE IS (Child)
        As%ChildId = -2
        As%BaseId  = -1

        IF ( ANY (LBOUND(As)      .NE. (/1,1/) ) )             ERROR STOP 30
        IF ( ANY (SHAPE(As)       .NE. (/2,2/) ) )             ERROR STOP 32

        IF ( ANY(As%ChildId .NE. -2) ) ERROR STOP 41
        IF ( ANY(As%BaseId  .NE. -1) ) ERROR STOP 42
        IF ( ANY(As%GetID()      .NE. -2) ) ERROR STOP 33
        IF ( ANY(As%Base%GetID() .NE. -1) ) ERROR STOP 34

        ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
          IF ( ANY(As0 .NE. -2) ) ERROR STOP 41
          IF ( ANY(As1 .NE. -1) ) ERROR STOP 42
        END ASSOCIATE

        ASSOCIATE ( As2 => As%Base )
          IF ( ANY(As2%GetID() .NE. -1 )) ERROR STOP 50
        END ASSOCIATE

        ASSOCIATE ( As2 => As%Base )
          IF ( ANY(As2%GetID() .NE. -1)) ERROR STOP 52
        END ASSOCIATE

    CLASS DEFAULT
      STOP 61
    END SELECT

  END ASSOCIATE


  END SUBROUTINE
  END