! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  FuncArgAlloc.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncArgAlloc
!*
!*  DATE                       : Nov. 02, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The associating entity associating to an allocatable is used as actual argument
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Foundation
    END TYPE

    TYPE, EXTENDS(Foundation) :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      TYPE(Foundation), ALLOCATABLE :: FdComp
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

  END MODULE

  PROGRAM FuncArgAlloc
  USE M
  TYPE(Child), ALLOCATABLE  :: V

  ALLOCATE(V)

  ASSOCIATE ( As => V  )
    ALLOCATE(As%FdComp, SOURCE=Foundation())
    PRINT*, Func(As)
    IF ( As.ChildId .NE. -2 ) STOP 22
  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Base)              :: Arg
    CHARACTER(3)  :: Func

    SELECT TYPE (Arg)
      TYPE IS (Child)
        Arg%ChildId = -2
      CLASS DEFAULT
        STOP 11
    END SELECT
    Func = "OK!"

  END FUNCTION

  END
