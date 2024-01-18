! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrCompSec.f
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
!*   The selector is a poly component array section
!*    (ICE-297844)
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

    TYPE :: Test(K2,N2)    ! (4,20)
        INTEGER, KIND :: K2
        INTEGER, LEN  :: N2
      CLASS(*), POINTER :: ChildArr(:,:,:)
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


  PROGRAM SltArrCompSec
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)), TARGET :: Arr(2:19, 2:19, 2:19)
  TYPE(Test(4,20)) :: V

  V%ChildArr => Arr

  SELECT TYPE ( V=>RESHAPE((/V%ChildArr(2:19:2, 2:19:2, 2:19:2)/), (/9,9,9/)) )
    CLASS DEFAULT
      SELECT TYPE (V=>V)
      CLASS IS (Zero(4,*))
      SELECT TYPE (V)
        TYPE IS (Child(4,*))
          IF ( ANY (LBOUND(V)     .NE. (/1, 1, 1/) ) )    STOP 30
          IF ( ANY (UBOUND(V)     .NE. (/9, 9, 9/) ) ) STOP 31
          IF ( ANY (SHAPE(V)      .NE. (/9, 9, 9/) ) ) STOP 32

          IF ( ANY(V%Base%GetId() .NE. 1) ) STOP 34
          IF ( ANY(V%GetId()      .NE. 2) ) STOP 35
          IF ( ANY(V%BaseId       .NE. 1) ) STOP 36
          IF ( ANY(V%ChildId      .NE. 2) ) STOP 37

          CALL V(2,2,2)%SetId(V)
          CALL V(2,2,2)%Base%SetId(V%Base)

          IF ( ANY(V%Base%GetId() .NE. -1 ) ) STOP 44
          IF ( ANY(V%GetId()      .NE. -2 ) ) STOP 45
          IF ( ANY(V%BaseId       .NE. -1 ) ) STOP 46
          IF ( ANY(V%ChildId      .NE. -2 ) ) STOP 47

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

  END

