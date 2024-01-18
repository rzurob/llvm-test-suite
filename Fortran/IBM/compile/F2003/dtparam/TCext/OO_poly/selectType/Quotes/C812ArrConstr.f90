! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_poly/selectType/Quotes/C812ArrConstr.f
! opt variations: -qnol -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C812
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
!*    The selector is an array constructor, the corresponding associate name
!*    appears in variable denifition context
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE :: Base(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (20,4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(*,4)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(*,4)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE


  PROGRAM C812ArrConstr
  USE M
  IMPLICIT NONE
  CLASS(*), ALLOCATABLE :: AllocV

  ALLOCATE( Child(20,4) :: AllocV)

  SELECT TYPE ( As => (/AllocV, AllocV, AllocV/) )
    TYPE IS (INTEGER)
      STOP 20
    CLASS DEFAULT
      STOP 30
    TYPE IS (Child(*,4))
      As = Child(20,4)(ChildId = -2)
      STOP 50
  END SELECT
  STOP 40

  END


