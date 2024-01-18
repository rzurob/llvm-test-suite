! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/EvalFstArrPolySec.f
! opt variations: -qnok -ql -qreuse=none

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

    TYPE, ABSTRACT ::  Zero(K1)    ! (4)
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
    TYPE IS (Child(4))
      IF ( ANY(As%GetId() .NE. -2) )      STOP 41
      IF ( ANY(As%Base%GetId() .NE. -1) ) STOP 42
      IF ( ANY(SHAPE(As) .NE. (/4/) ) )   STOP 43
      IF ( ANY(LBOUND(As) .NE. 1 ))       STOP 45
    CLASS IS (Base(4))
      STOP 20
  END SELECT


  CONTAINS

  FUNCTION Fun()
  CLASS(Child(4)), POINTER :: Fun
    ALLOCATE( Fun )
    SELECT TYPE(Fun)
    TYPE IS (Child(4))
      FUN = Child(4)(BaseId=-1, ChildId=-2)
    END SELECT
  END FUNCTION

  END



