! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/FuncArgPolyAlloc.f
! *********************************************************************
!*  ===================================================================
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
!*    The poly associating entity is used as actual argument
!*   The associated entity is poly allocatables
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND            :: K1
      INTEGER(K1)              :: BaseId = 1
      CLASS(Base(K1)), POINTER :: BaseComp => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      CLASS(Child(K1)),  POINTER :: ChildComp => NULL()
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

  PROGRAM FuncArgPolyAlloc
  USE M
  CLASS(*),     ALLOCATABLE :: V1
  CLASS(Base(4)),  ALLOCATABLE :: V2
  CLASS(Child(4)), ALLOCATABLE :: V3

  ALLOCATE(V3, SOURCE=Child(4)(BaseId=-1, ChildId=-2))
  ALLOCATE(V2, SOURCE=V3)
  ALLOCATE(V1, SOURCE=V2)

  ASSOCIATE ( As => V1 )
  ASSOCIATE ( As1 => Func(As) )
    SELECT TYPE (As1)
      TYPE IS (Child(4))
        IF ( As1.GetId()      .NE. -2 ) ERROR STOP 30
        IF ( As1.Base%GetId() .NE. -1 ) ERROR STOP 31
      CLASS DEFAULT
        STOP 32
    END SELECT
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => V2 )
  ASSOCIATE ( As1 => Func(As) )
    SELECT TYPE (As1)
      TYPE IS (Child(4))
        IF ( As1.GetId()      .NE. -2 ) ERROR STOP 40
        IF ( As1.Base%GetId() .NE. -1 ) ERROR STOP 41
      CLASS DEFAULT
        STOP 42
    END SELECT
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => V3 )
  ASSOCIATE ( As1 => Func(As) )
    SELECT TYPE (As1)
      TYPE IS (Child(4))
        IF ( As1.GetId()      .NE. -2 ) ERROR STOP 50
        IF ( As1.Base%GetId() .NE. -1 ) ERROR STOP 51
      CLASS DEFAULT
        STOP 52
    END SELECT
  END ASSOCIATE
  END ASSOCIATE

  CONTAINS


  FUNCTION Func(Arg)
    CLASS(*), TARGET      :: Arg
    CLASS(*), ALLOCATABLE :: Func

    ALLOCATE(Func, SOURCE=Arg)

  END FUNCTION

  END
