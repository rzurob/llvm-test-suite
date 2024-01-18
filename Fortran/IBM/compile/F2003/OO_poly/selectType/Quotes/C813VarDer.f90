! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp C813VarDer.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C813VarDer
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C813
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
!*    The selector is a var of derived type
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Ground
    END TYPE

    TYPE, EXTENDS(Ground) :: Base
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

  PROGRAM C813VarDer
  USE M
  IMPLICIT NONE

  CLASS(Child), ALLOCATABLE :: Var

  ALLOCATE(Var, SOURCE=Child(BaseId=-1, ChildId=-2) )

  SELECT TYPE (  As => Var%Base)
    TYPE IS (Base)
      STOP 50
    CLASS DEFAULT
      STOP 30
  END SELECT

  SELECT TYPE (Var)
    CLASS IS (Child)
      SELECT TYPE (  As => Var%Base)
        TYPE IS (Base)
          STOP 50
        CLASS DEFAULT
          STOP 30
      END SELECT
  END SELECT

  ASSOCIATE( As => Var%Base )
  SELECT TYPE (  As => As)
    TYPE IS (Base)
      STOP 50
    CLASS DEFAULT
      STOP 30
  END SELECT
  END ASSOCIATE

  END

