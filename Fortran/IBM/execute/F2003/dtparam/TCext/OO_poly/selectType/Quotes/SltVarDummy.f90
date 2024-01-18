! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltVarDummy.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 16, 2004
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
!*   The selector is a poly dummy.
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE

    TYPE, EXTENDS(Zero) :: Base    ! (4)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE Sub(Arg1, Arg2)
    CLASS(*) Arg1, Arg2
      SELECT TYPE (Arg1)
        TYPE IS(Child(4))
          SELECT TYPE (Arg2)
            CLASS IS (Base(4))
              STOP 30
            TYPE IS (Child(4))
              Arg2 = Arg1
            CLASS DEFAULT
              STOP 20
          END SELECT
        CLASS DEFAULT
          STOP 21
      END SELECT
    END SUBROUTINE

  END MODULE

  PROGRAM SltVarDummy
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr

  ALLOCATE(Child(4) :: Ptr)

  CALL Sub(Child(4)(BaseId=-1, ChildId=-2), Ptr)

  SELECT TYPE ( As => Ptr )
    CLASS DEFAULT
      STOP 20
    TYPE IS (CHARACTER(*))
      STOP 21
    TYPE IS (Base(4))
      STOP 22
    CLASS IS (Base(4))
      STOP 23
    CLASS is (Child(4))
      STOP 24
    TYPE IS (Child(4))
      IF ( As%BaseId       .NE. -1 ) STOP 31
      IF ( As%ChildId      .NE. -2 ) STOP 32
      IF ( As%Base%GetId() .NE. -1 ) STOP 33
      IF ( As%GetId()      .NE. -2 ) STOP 34

  END SELECT

  END
