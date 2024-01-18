! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 18, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!*   The selector is a function call returing a poly array pointer
!*   The function is a dummy procedure
!*    (ICE)
!*    (Wrong result-301404)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


 MODULE M

    TYPE  :: Zero
    CONTAINS
      PROCEDURE, NoPASS   :: SetId
      PROCEDURE, NoPASS   :: ReturnChild
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
   !  PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
   !  PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base)  :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetId(Arg)
    CLASS(Base)  :: Arg(:,:)
      SELECT TYPE (Arg)
        TYPE IS (Base)
          Arg%BaseId =  -Arg%BaseId
        TYPE IS (Child)
          Arg%ChildId =  -Arg%ChildId
      END SELECT
    END SUBROUTINE

    FUNCTION ReturnChild(Arg)
    CLASS(Zero), INTENT(IN) :: Arg(:,:)
    Type(Child)   :: ReturnChild(SIZE(Arg, 1), SIZE(Arg, 2))
      SELECT TYPE(Arg)
        TYPE IS (Child)
          ReturnChild = Arg
        CLASS DEFAULT
         ! STOP 111
      END SELECT
    END FUNCTION

    SUBROUTINE SetChildId(Arg)
    CLASS(Child)  :: Arg(:,:)
      Arg%ChildId = -Arg%ChildId
    END SUBROUTINE

    FUNCTION Obj(Arg, Func)
    IMPLICIT NONE
    CLASS(Zero), INTENT(IN) :: Arg(:,:)
    PROCEDURE(ReturnChild)  :: Func
    CLASS(*), POINTER :: Obj(:,:)
      ALLOCATE(Child :: Obj(SIZE(Arg, 1), SIZE(Arg, 2)))
      SELECT TYPE ( Obj)
        TYPE IS (Child)
          Obj = Func(Arg)
        CLASS DEFAULT
          STOP 111
      END SELECT
    END FUNCTION

  END MODULE


  PROGRAM SltArrFuncPtr1
  USE M
  IMPLICIT NONE
  TYPE(Child) :: V(3,3)
  INTEGER :: B1(3)=(/1,2,3/)
  INTEGER :: B2(3)=(/1,2,3/)

  SELECT TYPE ( As => Obj(V, ReturnChild) )
  CLASS DEFAULT

      SELECT TYPE (As)
        TYPE IS (Child)

          IF ( ANY (SHAPE(As) .NE. (/3,3/) )) ERROR STOP 20
          IF ( ANY (As%Base%GetId() .NE. 1 )) ERROR STOP 34
          IF ( ANY (As%GetId()      .NE. 2 )) ERROR STOP 35
          IF ( ANY (As%BaseId       .NE. 1 )) ERROR STOP 36
          IF ( ANY (As%ChildId      .NE. 2 )) ERROR STOP 37

          CALL As%SetId(As)
          CALL As%SetId(As%Base)

          IF ( ANY (As%Base%GetId() .NE. -1 )) ERROR STOP 44
          IF ( ANY (As%GetId()      .NE. -2 )) ERROR STOP 45
          IF ( ANY (As%BaseId       .NE. -1 )) ERROR STOP 46
          IF ( ANY (As%ChildId      .NE. -2 )) ERROR STOP 47

       CLASS DEFAULT
          STOP 40
       TYPE is (Base)
          STOP 32
       TYPE IS (Zero)
          STOP 38
      END SELECT

  END SELECT

  END



