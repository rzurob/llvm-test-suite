! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltFuncPolyZero.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltFuncPolyZero
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
!*   The selector is a poly func call returning entity of zero size.
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero
    END TYPE

    TYPE, EXTENDS(Zero) :: Base
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = 2
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = 1
    END FUNCTION

    FUNCTION Ful(Arg)
    CLASS(Base), TARGET  :: Arg
    CLASS(*),    POINTER :: Ful
      Ful => Arg
    END FUNCTION

  END MODULE

  PROGRAM SltFuncPolyZero
  USE M
  IMPLICIT  NONE

  INTERFACE
    FUNCTION Func(Arg)
      CLASS(*) :: Arg
      CLASS(*), ALLOCATABLE :: Func
    END FUNCTION
  END INTERFACE


  SELECT TYPE ( As => Func(Zero()))
    CLASS DEFAULT
      STOP 20
    CLASS is (Base)
      STOP 21
    TYPE is (INTEGER(1))
      STOP 22
    CLASS is (Child)
      STOP 23
    TYPE is (Child)
      STOP 24
    TYPE is (Zero)
      IF ( SIZEOF(As) .NE. 0 ) STOP 25
  END SELECT

  SELECT TYPE ( As => Func(Base()))
    CLASS DEFAULT
      STOP 30
    CLASS is (Base)
      STOP 31
    TYPE is (INTEGER(8))
      STOP 32
    CLASS is (Child)
      STOP 33
    TYPE is (Base)
      IF ( SIZEOF(As) .NE. 0 ) STOP 34
      IF ( As%GetId() .NE. 1 ) STOP 35
    TYPE is (Zero)
      STOP 36
  END SELECT

  SELECT TYPE ( As => Func(Child()))
    CLASS DEFAULT
      STOP 40
    CLASS is (Base)
      STOP 41
    TYPE is (INTEGER(8))
      STOP 42
    CLASS is (Child)
      IF ( SIZEOF(As)      .NE. 0 ) STOP 43
      IF ( As%Base%GetId() .NE. 1 ) STOP 44
      IF ( As%GetId()      .NE. 2 ) STOP 45
    TYPE is (Base)
      STOP 46
    TYPE is (Zero)
      STOP 47
  END SELECT


  END

  FUNCTION Func(Arg)
  USE M
  CLASS(*) :: Arg
  CLASS(*), ALLOCATABLE :: Func

  ALLOCATE(Func, SOURCE=Arg)

  END FUNCTION
