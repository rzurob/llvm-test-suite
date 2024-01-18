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
!*    The selector is a func call returning a poly array pointer
!*    of derived type
!*    ( ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
      CLASS(*),  POINTER :: BaseComp(:,:) => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      CLASS(*),  POINTER :: ChildComp(:, :) => NULL()
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
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM FunRetPolyPtrArr
  USE M
  TYPE(Child), TARGET :: V(2, 2) = Child(BaseId= -1, ChildId=-2 )

  ASSOCIATE ( As => Func( V ) )

    SELECT TYPE ( As )
      TYPE IS (Child)
        IF( ANY(As%GetId()  .NE. -2 )) STOP 46

        SELECT TYPE(As => As(1,1)%ChildComp)
        TYPE IS (Child)
          IF( ANY(As%GetID() .NE. -2) ) STOP 47
        END SELECT

        IF ( .NOT. ASSOCIATED(As(1,2)%BaseComp,  V) )  STOP 48
        IF ( .NOT. ASSOCIATED(As(2,1)%ChildComp, V) )  STOP 49

        SELECT TYPE ( As1 => As(2,2)%BaseComp )
        TYPE IS (Child)
          IF ( ANY( As1%BaseId  .NE. RESHAPE((/-1, -1, -1, -1/),(/2,2/)) ) ) STOP 74
          IF ( ANY( As1%ChildId .NE. RESHAPE((/-2, -2, -2, -2/),(/2,2/)) ) ) STOP 73
        CLASS DEFAULT
            STOP 70
        END SELECT

        SELECT TYPE ( As1 => As(2,1)%ChildComp )
          TYPE IS (Child)
            IF ( ANY( As1%ChildId .NE. RESHAPE((/-2, -2, -2, -2/),(/2,2/)) ) ) STOP 72
          CLASS DEFAULT
            STOP 71
        END SELECT

        IF ( .NOT. SAME_TYPE_AS(As, Child()) )  STOP 53

      CLASS DEFAULT
        STOP 80
    END SELECT

  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child), TARGET  :: Arg(:,:)
    CLASS(*),  POINTER    :: Func(:,:)

    FUNC => Arg
    SELECT TYPE (As => Func )
      TYPE IS (Child)
        DO i = 1, SIZE(Arg(:,1))
        DO j = 1, SIZE(Arg(1,:))
          AS(i, j)%BaseComp  => Arg
          AS(i, j)%ChildComp => Arg
        END DO
        END DO
      CLASS DEFAULT
        STOP 77
    END SELECT

  END FUNCTION

  END

