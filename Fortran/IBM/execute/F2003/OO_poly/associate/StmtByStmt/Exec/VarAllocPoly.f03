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
!*    The selector is a allocatable polymorphic  variable
!*    The selector is a allocatable unlimited polymorphic  variable
!*    (ICE @  DEALLOCATE(As%BaseComp)
!*    (295070)
!*    (298103-Segment fault on allocating V )
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      TYPE(Base), ALLOCATABLE :: BaseComp
    CONTAINS
      PROCEDURE, NOPASS :: PrintType => PrintChild
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    SUBROUTINE PrintChild()
      PRINT *,'Child'
    END SUBROUTINE

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

  PROGRAM VarAllocPoly
  USE M
  Class(Base), ALLOCATABLE :: U
  Class(*),    ALLOCATABLE :: V

  ALLOCATE( Child :: U)
  ASSOCIATE ( As => U )
    IF ( As%GetID() .NE. 2) ERROR STOP 50
    IF ( As%BaseId  .NE. 1) ERROR STOP 51

    ASSOCIATE ( As1 => As%BaseId )
       IF ( As1 .NE. 1) ERROR STOP 52
    END ASSOCIATE

    IF ( .NOT. SAME_TYPE_AS(As, Child(BaseComp=NULL())) ) ERROR STOP 53

    SELECT TYPE ( As )
      TYPE IS (Child)
        ALLOCATE(As%BaseComp)
        IF ( As%BaseComp%BaseId  .NE. 1) ERROR STOP 54
        IF ( As%BaseComp%GetId() .NE. 1) ERROR STOP 55
        DEALLOCATE(As%BaseComp)

        IF ( As%GetID() .NE. 2) ERROR STOP 60
        IF ( As%ChildId .NE. 2) ERROR STOP 61

      CLASS DEFAULT
        STOP 70
    END SELECT

    DEALLOCATE(U)
  END ASSOCIATE

  !Unlimited poly
  ALLOCATE (V, SOURCE=Child(BaseComp=NULL()) )
  ASSOCIATE ( As => V )

    SELECT TYPE ( As )
      TYPE IS (Child)
        IF ( As%GetID() .NE. 2)      ERROR STOP 49
        IF ( As%ChildId .NE. 2)      ERROR STOP 50
        IF ( As%BaseId  .NE. 1)      ERROR STOP 51
        IF ( As%Base%GetId() .NE. 1) ERROR STOP 52

        ASSOCIATE ( As1 => As%BaseId )
         IF ( As1 .NE. 1) ERROR STOP 52
        END ASSOCIATE

        IF ( .NOT. SAME_TYPE_AS(As, Child(BaseComp=NULL())) ) ERROR STOP 53

        ALLOCATE(As%BaseComp)
        IF ( As%BaseComp%BaseId  .NE. 1) ERROR STOP 54
        IF ( As%BaseComp%GetId() .NE. 1) ERROR STOP 55
        DEALLOCATE(As%BaseComp)

        IF ( As%GetID() .NE. 2) ERROR STOP 60
        IF ( As%ChildId .NE. 2) ERROR STOP 61

      CLASS DEFAULT
        STOP 70
    END SELECT

    DEALLOCATE(V)


  END ASSOCIATE


  END