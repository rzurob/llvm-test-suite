! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/SltArrPtr.f
! opt variations: -qnok -qnol -qnodeferredlp

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
!*   The selector is an array(zero size) pointer
!*    (297764)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base(K2,N2)    ! (4,20,4,20)
        INTEGER, KIND :: K2
        INTEGER, LEN  :: N2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child(K3,N3)    ! (4,20,4,20,4,20)
        INTEGER, KIND :: K3
        INTEGER, LEN  :: N3
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*,4,*,4,*)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = 2
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*,4,*)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = 1
    END FUNCTION

  END MODULE


  PROGRAM SltArrPtr
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20,4,20,4,20)), TARGET :: Tar

  CLASS(Zero(4,:)), POINTER :: Arr(:)

  ALLOCATE(Child(4,20,4,20,4,20) :: Arr(1111))

  SELECT TYPE ( As => Arr(::2))
    CLASS IS (Child(4,*,4,*,4,*))
      SELECT TYPE (As => As(::3))
        CLASS DEFAULT
        SELECT TYPE ( As )
          TYPE IS (Child(4,*,4,*,4,*))

            IF ( SIZE(As)   .NE. 186 )          STOP 42
            IF ( SIZEOF(As) .NE. 0 )            STOP 43
            IF ( ANY(As%Base%GetId() .NE. 1 ) ) STOP 44
            IF ( ANY(As%GetId()      .NE. 2 ) ) STOP 45

         CLASS DEFAULT
            STOP 40
        END SELECT
      END SELECT

    TYPE is (Base(4,*,4,*))
      STOP 32
    TYPE IS (Zero(4,*))
      STOP 38

  END SELECT

  END

