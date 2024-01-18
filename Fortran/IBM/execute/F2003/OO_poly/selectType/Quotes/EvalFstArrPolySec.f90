! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: EvalFstArrPolySec.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : EvalFstArrPolySec
!*
!*  DATE                       : Dec. 13, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : selector expression
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
!*   The selector expr is evaluated first
!*   Poly array section constructed with  funcion calls
!*    (Comp failed: 296969)
!*    (ICE on poly array constructor as source of allocate-298490)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE, ABSTRACT ::  Zero
    END TYPE

    TYPE, EXTENDS(Zero) :: Base
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



  PROGRAM EvalFstArrPolySec
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr(:)
  INTEGER :: i

  ALLOCATE( Ptr(10), SOURCE=(/(Fun(), i=1, 10)/) )
  SELECT TYPE ( As => Ptr(2:9:2) )
    CLASS DEFAULT
      STOP 30
    TYPE IS (Child)
      IF ( ANY(As%GetId() .NE. -2) )      STOP 41
      IF ( ANY(As%Base%GetId() .NE. -1) ) STOP 42
      IF ( ANY(SHAPE(As) .NE. (/4/) ) )   STOP 43
      IF ( ANY(LBOUND(As) .NE. 1 ))       STOP 45
    CLASS IS (Base)
      STOP 20
  END SELECT


  CONTAINS

  FUNCTION Fun()
  CLASS(Child), POINTER :: Fun
    ALLOCATE( Fun )
    SELECT TYPE(Fun)
    TYPE IS (Child)
      FUN = Child(BaseId=-1, ChildId=-2)
    END SELECT
  END FUNCTION

  END



