! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/FuncArgPolyPtr.f
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
!*    The associated entity is a pointer
!*    (Comp failed: ICE)
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

    FUNCTION GetChildId(Arg)
    CLASS(Child(4)) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base(4))  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM FuncArgPolyPtr
  USE M
  CLASS(*),     ALLOCATABLE :: V1
  CLASS(Base(4)),  ALLOCATABLE :: V2
  CLASS(Child(4)), ALLOCATABLE :: V3

  ALLOCATE(V1, SOURCE=GenChild(-1, -2))
  SELECT TYPE (V1)
    TYPE IS (Child(4))
      ALLOCATE(V2, SOURCE=V1)
    CLASS DEFAULT
      STOP 20
  END SELECT
  SELECT TYPE (V2)
    TYPE IS (Child(4))
      ALLOCATE(V3, SOURCE=V2)
    CLASS DEFAULT
      STOP 21
  END SELECT

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

  FUNCTION GenChild(Arg1, Arg2)
  CLASS(*), ALLOCATABLE :: GenChild
  INTEGER               :: Arg1, Arg2
    ALLOCATE(GenChild, SOURCE=Child(4)(BaseId=Arg1, ChildId=Arg2))
  END FUNCTION

  FUNCTION Func(Arg)
    CLASS(*), TARGET  :: Arg
    CLASS(*), POINTER  :: Func

    Func => Arg

  END FUNCTION

  END
