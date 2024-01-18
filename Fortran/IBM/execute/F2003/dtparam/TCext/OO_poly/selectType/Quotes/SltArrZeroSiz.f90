! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Quotes/SltArrZeroSiz.f
! opt variations: -qnok -qnol

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
!*   The selector is a zero size  array
!*    (ICE-297864)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4,20)
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = 2
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = 1
    END FUNCTION

  END MODULE


  PROGRAM SltArrZeroSiz
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)) :: Arr(18, 18, 18)

  CALL Sub(Arr)

  CONTAINS

  SUBROUTINE Sub(Arr)
  CLASS(*) :: Arr(2:, 2:, 2:)
  LOGICAL  :: Mask(18, 18, 18)=.TRUE.

  Mask(::2, ::2, ::2) = .FALSE.

  SELECT TYPE ( Arr )
    CLASS DEFAULT
      SELECT TYPE (Arr => MERGE(Arr, Arr, Mask) )
      CLASS IS (Zero(4,*))
      SELECT TYPE (Ptr => Arr)
        TYPE IS (Child(4,*))

          IF ( ANY (LBOUND(Ptr)   .NE. (/1, 1, 1/)  ) ) STOP 30
          IF ( ANY (UBOUND(Ptr)   .NE. (/18,18,18/) ) ) STOP 31
          IF ( ANY (SHAPE(Ptr)    .NE. (/18,18,18/) ) ) STOP 32

          IF ( ANY(Ptr%Base%GetId() .NE. 1) ) STOP 34
          IF ( ANY(Ptr%GetId()      .NE. 2) ) STOP 35

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

