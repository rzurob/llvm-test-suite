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



  PROGRAM EvalFstArrPoly
  USE M
  IMPLICIT NONE

  SELECT TYPE ( As => (/Fun(), Fun(), Fun()/) )
    CLASS DEFAULT
      STOP 30
    TYPE IS (Child)
      IF ( ANY(As%GetId() .NE. -2) )      ERROR STOP 41
      IF ( ANY(As%Base%GetId() .NE. -1) ) ERROR STOP 42
      IF ( ANY(SHAPE(As) .NE. (/3/) ) )   ERROR STOP 43
  END SELECT


  CONTAINS

  FUNCTION Fun()
  CLASS(Child), POINTER :: Fun

    ALLOCATE( Fun )
    SELECT TYPE (Fun)
      TYPE IS (Child)
        Fun%BaseId=-1
        Fun%ChildId=-2
    END SELECT

  END FUNCTION

  END

