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
!*    The selector is a polymorphic dummy
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
      CLASS(Base),  POINTER :: BaseComp
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

  PROGRAM VarDummyPoly
  USE M
  CLASS(Base), POINTER :: U

  ALLOCATE(Child :: U)
  CALL Sub(U, U)

  CONTAINS

  SUBROUTINE Sub(Arg1, Arg2)
  CLASS(Base) :: Arg1
  CLASS(*)    :: Arg2

  ASSOCIATE ( As => Arg1 )
    IF ( As%GetID() .NE. 2) ERROR STOP 50
    IF ( As%BaseId  .NE. 1) ERROR STOP 51

    ASSOCIATE ( As1 => As%BaseId )
       IF ( As1 .NE. 1) ERROR STOP 52
    END ASSOCIATE

    IF ( .NOT. SAME_TYPE_AS(As, Child(BaseComp=NULL())) ) ERROR STOP 53

    SELECT TYPE ( As )
      TYPE IS (Child)
        ALLOCATE(Child :: As%BaseComp)

        IF ( As%BaseComp%BaseId  .NE. 1) ERROR STOP 54
        IF ( As%BaseComp%GetId() .NE. 2) ERROR STOP 55

        IF ( As%GetID() .NE. 2) ERROR STOP 56
        IF ( As%ChildId .NE. 2) ERROR STOP 57

        As%BaseId  = -1  !Test Arg2
        As%ChildId = -2

      CLASS DEFAULT
        STOP 70
    END SELECT

  END ASSOCIATE

  !Unlimited poly Arg2
  ASSOCIATE ( As => Arg2 )

    SELECT TYPE ( As )
      TYPE IS (Child)
        IF ( As%GetID() .NE. -2)      ERROR STOP 40
        IF ( As%ChildId .NE. -2)      ERROR STOP 41
        IF ( As%BaseId  .NE. -1)      ERROR STOP 42
        IF ( As%Base%GetId() .NE. -1) ERROR STOP 43

        ASSOCIATE ( As1 => As%BaseId )
         IF ( As1 .NE. -1) ERROR STOP 44
        END ASSOCIATE

        IF ( .NOT. SAME_TYPE_AS(As, Child(BaseComp=NULL())) ) ERROR STOP 45
        IF ( .NOT. SAME_TYPE_AS(As%BaseComp, As) ) ERROR STOP 46

        ALLOCATE(As%BaseComp)
        IF ( As%BaseComp%BaseId  .NE. 1) ERROR STOP 47
        IF ( As%BaseComp%GetId() .NE. 1) ERROR STOP 48
        DEALLOCATE(As%BaseComp)

        IF ( As%GetID() .NE. -2) ERROR STOP 49
        IF ( As%ChildId .NE. -2) ERROR STOP 30

      CLASS DEFAULT
        STOP 80
    END SELECT

  END ASSOCIATE

  END SUBROUTINE

  END