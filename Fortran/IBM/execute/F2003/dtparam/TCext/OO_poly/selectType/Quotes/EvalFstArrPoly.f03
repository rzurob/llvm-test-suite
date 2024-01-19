! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/EvalFstArrPoly.f
! opt variations: -qnok -qnol -qdeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 9, 2004
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
!*   Poly array constructed with  funcion calls
!*    (ICe)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  MODULE M

    TYPE, ABSTRACT ::  Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero) :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE



  PROGRAM EvalFstArrPoly
  USE M
  IMPLICIT NONE

  SELECT TYPE ( As => (/Fun(), Fun(), Fun()/) )
    CLASS DEFAULT
      STOP 30
    TYPE IS (Child(4,*))
      IF ( ANY(As%GetId() .NE. -2) )      ERROR STOP 41
      IF ( ANY(As%Base%GetId() .NE. -1) ) ERROR STOP 42
      IF ( ANY(SHAPE(As) .NE. (/3/) ) )   ERROR STOP 43
  END SELECT


  CONTAINS

  FUNCTION Fun()
  CLASS(Child(4,20)), POINTER :: Fun

    ALLOCATE( Fun )
    SELECT TYPE (Fun)
      TYPE IS (Child(4,*))
        Fun%BaseId=-1
        Fun%ChildId=-2
    END SELECT

  END FUNCTION

  END

