! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrFuncVec.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 17, 2005
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
!*   The selector is an array from function call with vector subscript
!*    (Wrong shape&ICE-298279)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


 MODULE M

    TYPE  :: Zero(K1)    ! (4)
        INTEGER, KIND :: K1
    CONTAINS
      PROCEDURE, NoPASS   :: SetId
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
   !  PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
   !  PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4))  :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetId(Arg)
    CLASS(Base(4))  :: Arg(:,:)
      SELECT TYPE (Arg)
        TYPE IS (Base(4))
          Arg%BaseId =  -Arg%BaseId
        TYPE IS (Child(4))
          Arg%ChildId =  -Arg%ChildId
      END SELECT
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4))  :: Arg(:,:)
      Arg%ChildId = -Arg%ChildId
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrFuncVes
  USE M
  IMPLICIT NONE
  TYPE(Child(4)) :: V(3,3)
  INTEGER :: B1(3)=(/1,2,3/)
  INTEGER :: B2(3)=(/1,2,3/)

  SELECT TYPE ( As => Fun(V) )
  CLASS DEFAULT
  SELECT TYPE ( As => As(B1, B2) )
  CLASS DEFAULT
  SELECT TYPE ( As )
  CLASS DEFAULT
    CLASS IS (Zero(4))
      SELECT TYPE (As)
        TYPE IS (Child(4))
          IF ( ANY (SHAPE(As) .NE. (/3,3/) )) STOP 20
          IF ( ANY (As%Base%GetId() .NE. 1 )) STOP 34
          IF ( ANY (As%GetId()      .NE. 2 )) STOP 35
          IF ( ANY (As%BaseId       .NE. 1 )) STOP 36
          IF ( ANY (As%ChildId      .NE. 2 )) STOP 37

          CALL As%SetId(As)
          CALL As%SetId(As%Base)

          IF ( ANY (As%Base%GetId() .NE. -1 )) STOP 44
          IF ( ANY (As%GetId()      .NE. -2 )) STOP 45
          IF ( ANY (As%BaseId       .NE. -1 )) STOP 46
          IF ( ANY (As%ChildId      .NE. -2 )) STOP 47
       CLASS DEFAULT
          STOP 40
      END SELECT

    TYPE is (Base(4))
      STOP 32
    TYPE IS (Zero(4))
      STOP 38

  END SELECT
  END SELECT
  END SELECT

  CONTAINS

  FUNCTION Fun(Arg)
  CLASS(*) :: Arg(:,:)
  CLASS(*), POINTER :: Fun(:,:)
  INTEGER :: SubScript(2)

  SubScript = SHAPE(Arg)
  ALLOCATE(Fun(SubScript(1), SubScript(2)), SOURCE=Arg)

  END FUNCTION
  END



