! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrDummy.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 06, 2005
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
!*   The selector is a dummy array
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Zero(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      CLASS(Base(K1)), POINTER :: BasePtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
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
    CLASS(Base(4))  :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4))  :: Arg(:)
      SELECT TYPE (Arg)
        TYPE IS (Child(4))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrDummy
  USE M
  IMPLICIT NONE
  TYPE(Child(4)), TARGET :: Arr(1000)

  CALL Sub( Arr, Arr, SIZE(Arr) )

  CONTAINS

  SUBROUTINE Sub(Arr0, Arr1, N)
  CLASS(Zero(4)) :: Arr0(*), Arr1(*)
  INTEGER :: N

  SELECT TYPE ( As0 => Arr0 )
    CLASS IS (Child(4))
      SELECT TYPE (As1 => Arr1 )
        TYPE IS (Child(4))

          ! Can not quary size of assumed size array
          !  IF ( SIZE(As0)   .NE. SIZE(Arr1) ) ERROR STOP 42
          !  IF ( SIZE(As0)   .NE. SIZE(As1) ) ERROR STOP 42

          IF ( ANY(As0(:N)%Base%GetId() .NE. 1) ) ERROR STOP 34
          IF ( ANY(As0(:N)%GetId()      .NE. 2) ) ERROR STOP 35
          IF ( ANY(As0(:N)%BaseId       .NE. 1) ) ERROR STOP 36
          IF ( ANY(As0(:N)%ChildId      .NE. 2) ) ERROR STOP 37

          CALL As1(1)%SetId(As0(:N))
          CALL As1(1)%Base%SetId(As0(:N)%Base)

          IF ( ANY(As1(:N)%Base%GetId() .NE. -1 ) ) ERROR STOP 44
          IF ( ANY(As1(:N)%GetId()      .NE. -2 ) ) ERROR STOP 45
          IF ( ANY(As1(:N)%BaseId       .NE. -1 ) ) ERROR STOP 46
          IF ( ANY(As1(:N)%ChildId      .NE. -2 ) ) ERROR STOP 47

         CLASS DEFAULT
            STOP 40
      END SELECT

    TYPE is (Base(4))
      STOP 32
    TYPE IS (Zero(4))
      STOP 38

  END SELECT

  END SUBROUTINE

  END

