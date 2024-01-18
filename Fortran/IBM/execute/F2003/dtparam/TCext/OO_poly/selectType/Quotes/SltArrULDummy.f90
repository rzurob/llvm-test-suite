! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrULDummy.f
! opt variations: -qnok -qnol -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 07, 2005
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
!*   The selector is an  unlimited polymorphic dummy array
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4,*)) :: Arg(:,:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4,*))  :: Arg(:,:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4,*))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrULDummy
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)) :: Arr(18, 18,18)

  CALL Sub(Arr)

  CONTAINS

  SUBROUTINE Sub(Arr)
  CLASS(*) :: Arr(2:19, 2:19, 2:19)

  SELECT TYPE ( Arr )
    CLASS DEFAULT
      SELECT TYPE (Ptr=>Arr)
      CLASS IS (Zero(4,*))
      SELECT TYPE (Ptr)
        TYPE IS (Child(4,*))
          IF ( ANY (LBOUND(Ptr)     .NE. (/2, 2, 2/) ) )    ERROR STOP 30
          IF ( ANY (UBOUND(Ptr)     .NE. (/19, 19, 19/) ) ) ERROR STOP 31
          IF ( ANY (SHAPE(Ptr)      .NE. (/18, 18, 18/) ) ) ERROR STOP 32

          IF ( ANY(Ptr%Base%GetId() .NE. 1) ) ERROR STOP 34
          IF ( ANY(Ptr%GetId()      .NE. 2) ) ERROR STOP 35
          IF ( ANY(Ptr%BaseId       .NE. 1) ) ERROR STOP 36
          IF ( ANY(Ptr%ChildId      .NE. 2) ) ERROR STOP 37

          CALL Ptr(2,2,2)%SetId(Ptr)
          CALL Ptr(2,2,2)%Base%SetId(Ptr%Base)

          IF ( ANY(Ptr%Base%GetId() .NE. -1 ) ) ERROR STOP 44
          IF ( ANY(Ptr%GetId()      .NE. -2 ) ) ERROR STOP 45
          IF ( ANY(Ptr%BaseId       .NE. -1 ) ) ERROR STOP 46
          IF ( ANY(Ptr%ChildId      .NE. -2 ) ) ERROR STOP 47

       CLASS DEFAULT
          STOP 40
        CLASS is (Child(4,*))
          STOP 56
        TYPE is (Base(4,*))
          STOP 57
        TYPE IS (Zero(4,*))
          STOP 58
      END SELECT
      END SELECT

  END SELECT

  END SUBROUTINE

  END

