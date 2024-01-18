! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Quotes/SltVarZero.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 15, 2004
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
!*   The selector is a poly var of zero size
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
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
    INTEGER      :: GetChildId
      GetChildId = 2
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = 1
    END FUNCTION

  END MODULE

  PROGRAM SltVarZero
  USE M
  IMPLICIT NONE

  CLASS(*), ALLOCATABLE :: Var

  ALLOCATE(Child(4,20) :: Var)

  ASSOCIATE ( As => Var )
  SELECT TYPE (As)
    CLASS DEFAULT
      SELECT TYPE (As)
        CLASS DEFAULT
          STOP 20
        TYPE IS (CHARACTER(*))
          STOP 21
        TYPE IS (Base(4,*))
          STOP 22
        CLASS IS (Base(4,*))
          STOP 23
        CLASS is (Child(4,*))
          STOP 24
        TYPE IS (Child(4,*))
          IF ( As%Base%GetId() .NE. 1 ) ERROR STOP 34
          IF ( As%GetId()      .NE. 2 ) ERROR STOP 35
      END SELECT
  END SELECT
  END ASSOCIATE

  END
