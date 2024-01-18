! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ArrConstr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ArrConstr
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
!*    The selector an array constructor
!*    (Wrong bound-299594)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId
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

    SUBROUTINE SetId(Obj, Id)
    CLASS(Base)  :: Obj(:,:)
    INTEGER      :: Id

      SELECT TYPE (Obj)
      TYPE IS (Base)
        Obj%BaseId = Id
      TYPE IS (Child)
        Obj%ChildId = Id
      END SELECT
    END SUBROUTINE

  END MODULE

  PROGRAM ArrStrConstr
  USE M
  IMPLICIT NONE
  INTEGER :: i
  LOGICAL(8) :: Mask(2,2) = .TRUE.

  ASSOCIATE ( As => RESHAPE((/(Child(BaseId=i, ChildId=-i), i=1,4 )/), (/2,2/) ))

    IF ( ANY (LBOUND(As)      .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (SHAPE(As).NE. (/2,2/) ) )             STOP 32

    IF ( ANY (As%Base%GetID()      .NE. RESHAPE((/ 1, 2, 3, 4/), (/2,2/)) ) ) STOP 33
    IF ( ANY (As%GetID() .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) ) ) STOP 34

    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( ANY(As0 .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) ) ) STOP 41
       IF ( ANY(As1 .NE. RESHAPE((/ 1, 2, 3, 4/), (/2,2/)) ) ) STOP 42
    END ASSOCIATE

    ASSOCIATE ( As2 => As%Base )
      IF ( ANY(As2%GetID() .NE. RESHAPE((/ 1, 2, 3, 4/), (/2,2/)) )) STOP 50
    END ASSOCIATE

    ASSOCIATE (As =>  As%GetID())
      IF ( ANY(As .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) )) STOP 60
    END ASSOCIATE

    ASSOCIATE (As =>  As%Base%GetID())
      IF ( ANY(As .NE. RESHAPE((/ 1, 2, 3, 4/), (/2,2/)) )) STOP 70
    END ASSOCIATE

    ASSOCIATE ( As => MERGE(1, 2, 1>0))
      IF ( As .NE. 1 ) STOP 55
    END ASSOCIATE

    ASSOCIATE ( As => (MERGE(As, As, Mask )) )
      IF ( ANY (LBOUND(As) .NE. (/1,1/) ) )    STOP 50
      IF ( ANY (SHAPE(As)  .NE. (/2,2/) ) )    STOP 52
      IF ( ANY (As%BaseID  .NE. RESHAPE((/ 1, 2, 3, 4/), (/2,2/)) ) ) STOP 53
      IF ( ANY (As%ChildID .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) ) ) STOP 54
    END ASSOCIATE

    ! This part is not good
    CALL As%SetId(As%Base, 1)
    CALL As%SetId(As, 2)

    IF ( ANY (As%GetID()      .NE. RESHAPE((/2,2,2,2/), (/2,2/)) ) ) STOP 83
    IF ( ANY (As%Base%GetID() .NE. RESHAPE((/1,1,1,1/), (/2,2/)) ) ) STOP 84

  END ASSOCIATE

  END
