! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/selectType/Quotes/EvalFstFunc.f
! opt variations: -qnok -qnol -qreuse=none

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
!*    ()
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



  PROGRAM EvalFstFunc
  USE M
  IMPLICIT NONE

  LOGICAL :: Visited = .FALSE.
  TYPE(Child(4,20)), TARGET :: Tar


  SELECT TYPE ( As => Fun(Tar) )
    CLASS DEFAULT
      STOP 30
    TYPE IS (Child(4,*))
      IF( .NOT. Visited ) ERROR STOP 40
      IF ( As%GetId() .NE. 2 ) ERROR STOP 41
      IF ( As%Base%GetId() .NE. 1 ) ERROR STOP 42
    CLASS IS (Base(4,*))
      STOP 20
  END SELECT


  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(Child(4,*)), TARGET  :: Arg
  CLASS(*),    POINTER :: Fun
    Fun => Arg
    Visited = .TRUE.
  END FUNCTION

  END

