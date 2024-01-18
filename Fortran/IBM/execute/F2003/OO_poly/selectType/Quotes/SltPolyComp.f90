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

    TYPE, ABSTRACT :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM SltPolyComp
  USE M
  IMPLICIT NONE

  TYPE, EXTENDS(Child) :: DT
    CLASS(Child), POINTER :: PolyChild
  END TYPE

  CLASS(*), POINTER :: Var

  ALLOCATE(Var, SOURCE=DT(BaseId=-1,ChildId=-2,PolyChild=NULL()))
  SELECT TYPE (Var)
    CLASS IS (DT)
      Var%PolyChild => Var%Child
    CLASS DEFAULT
      STOP 22
  END SELECT

  CALL Sub(Var)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) ::  Arg

  SELECT TYPE (Arg)
    CLASS IS (DT)
      SELECT TYPE (As => Arg%PolyChild)
        CLASS DEFAULT
          STOP 20
        TYPE IS (DT)
           STOP 21
!       TYPE IS (Base)
!          STOP 22
!       CLASS IS (Base)
!         STOP 23
        CLASS is (Child)
          STOP 24
        TYPE IS (Child)
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
