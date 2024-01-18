! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltPolyComp.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltPolyComp.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltTypePolyComp
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
!*   The selector is a poly component of a var
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
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

  END MODULE

  PROGRAM SltPolyComp
  USE M
  IMPLICIT NONE

  TYPE, EXTENDS(Child) :: DT    ! (4)
    CLASS(Child(K1)), POINTER :: PolyChild
  END TYPE

  CLASS(*), POINTER :: Var

  ALLOCATE(Var, SOURCE=DT(4)(BaseId=-1,ChildId=-2,PolyChild=NULL()))
  SELECT TYPE (Var)
    CLASS IS (DT(4))
      Var%PolyChild => Var%Child
    CLASS DEFAULT
      STOP 22
  END SELECT

  CALL Sub(Var)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) ::  Arg

  SELECT TYPE (Arg)
    CLASS IS (DT(4))
      SELECT TYPE (As => Arg%PolyChild)
        CLASS DEFAULT
          STOP 20
        TYPE IS (DT(4))
           STOP 21
!       TYPE IS (Base)
!          STOP 22
!       CLASS IS (Base)
!         STOP 23
        CLASS is (Child(4))
          STOP 24
        TYPE IS (Child(4))
          IF ( As%BaseId       .NE. -1 ) STOP 31
          IF ( As%Base%BaseId  .NE. -1 ) STOP 32
          IF ( As%ChildId      .NE. -2 ) STOP 33
!         IF ( As%Base%GetId() .NE. -1 ) STOP 34 !C611
          IF ( As%GetId()      .NE. -2 ) STOP 35
      END SELECT
    CLASS DEFAULT
      STOP 99
  END SELECT

  END SUBROUTINE

  END
