! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltVarULAlloc.f
! opt variations: -qnok -qnol -qreuse=none

! *********************************************************************
!*  ===================================================================
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
!*   The selector is an unlimited  poly allocatable
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero(K1,N1)    ! (4,20)
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

  PROGRAM SltVarULAlloc
  USE M
  IMPLICIT  NONE

  CLASS(*), ALLOCATABLE :: Var , Vtype

  ALLOCATE(Vtype, SOURCE=Child(4,20)(BaseId=-1, ChildId=-2))
  SELECT TYPE ( AA => Vtype) !Vtype should be allocated first.
    CLASS DEFAULT
 !    DEALLOCATE(Vtype)
 !    ALLOCATE(Vtype, SOURCE=Child(BaseId=-1, ChildId=-2))
      ALLOCATE(Var, SOURCE=AA)

      SELECT TYPE ( Var )
        CLASS DEFAULT
          STOP 20
        CLASS is (Base(4,*))
          STOP 24
        TYPE is (INTEGER(1))
          STOP 24
        CLASS is (Child(4,*))
          IF ( Var%BaseId       .NE. -1 ) ERROR STOP 31
          IF ( Var%ChildId      .NE. -2 ) ERROR STOP 32
          IF ( Var%Base%GetId() .NE. -1 ) ERROR STOP 33
          IF ( Var%GetId()      .NE. -2 ) ERROR STOP 34
    END SELECT
  END SELECT

  END
