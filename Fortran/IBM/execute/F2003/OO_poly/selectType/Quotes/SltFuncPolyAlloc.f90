! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltFuncPolyAlloc.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltFuncPolyAlloc
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
!*   The selector is a poly func call returning an allocatable entity.
!*    (Core Dump)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SltFuncPolyAlloc
  IMPLICIT  NONE

  INTERFACE
    FUNCTION Func(Arg)
      CLASS(*) :: Arg
      CLASS(*), ALLOCATABLE :: Func
    END FUNCTION
  END INTERFACE


  SELECT TYPE ( As => Func(Func("")))
    CLASS DEFAULT
      STOP 20
    TYPE is (CHARACTER(*))
      IF ( LEN(As) .NE. 0 ) STOP 21
      IF ( As .NE. "" ) STOP 22
    TYPE is (INTEGER(1))
      STOP 23
    TYPE is (REAL)
      STOP 24
  END SELECT

  SELECT TYPE ( As => Func(Func(1_8)))
    CLASS DEFAULT
      STOP 30
    TYPE is (CHARACTER(*))
      STOP 31
    TYPE is (INTEGER(1))
      STOP 32
    TYPE is (REAL)
      STOP 33
    TYPE IS (INTEGER(4))
      STOP 34
    TYPE IS (INTEGER(8))
      IF ( As .NE. 1_8 ) STOP 35
  END SELECT



  END

  FUNCTION Func(Arg)
  CLASS(*) :: Arg
  CLASS(*), ALLOCATABLE :: Func

  ALLOCATE(Func, SOURCE=Arg)

  END FUNCTION
