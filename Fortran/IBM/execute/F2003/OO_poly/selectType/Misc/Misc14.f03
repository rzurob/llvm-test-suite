! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 12, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Wrong result-select type/allocate/polyEntity
!*    (295710)
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
      TYPE(Base) :: BaseComp = Base(0)
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

  PROGRAM  Misc14
  USE M

  IMPLICIT NONE

  CLASS(Base), ALLOCATABLE   :: U

    ALLOCATE(U, SOURCE=Child(BaseId=-1, ChildId=-2, BaseComp=Base(0)))

    SELECT TYPE ( U )
      TYPE IS ( Child )
        print*, U%BaseId ! should be -1
        print*, U%ChildId ! should be -2
        print*, U%BaseComp%BaseId ! should be 0
        IF (U%BaseId   .NE. -1) ERROR STOP 21
        IF (U%ChildId  .NE. -2) ERROR STOP 22
        IF (U%BaseComp%BaseId .NE. 0) ERROR STOP 20
    end select

  END


