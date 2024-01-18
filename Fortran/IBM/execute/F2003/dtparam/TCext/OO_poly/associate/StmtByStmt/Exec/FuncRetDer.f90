! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/FuncRetDer.f
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
!*    The selector is a func call returning a value of derived type
!*    (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      CLASS(Child(K1)),  POINTER :: ChildComp  => NULL()
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

  PROGRAM FunRetDer
  USE M

  ASSOCIATE ( As => Func(Child(4)(BaseId= -1, ChildId=-2 )) )
    IF ( As%GetID() .NE. -2) STOP 50
    IF ( As%BaseId  .NE. -1) STOP 51
    IF ( As%ChildId .NE. -2) STOP 61

    ASSOCIATE ( As1 => As%GetId() )
       IF ( As1 .NE. -2) STOP 52
    END ASSOCIATE

    IF ( .NOT. SAME_TYPE_AS(As, Child(4)()) ) STOP 53
    IF ( As%ChildComp%BaseId  .NE. 1)      STOP 54
    IF ( As%ChildComp%GetId() .NE. 2)      STOP 55
  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child(4)) :: Arg
    TYPE(Child(4))  :: Func

    Func = Arg
    ALLOCATE(FUNC%ChildComp)

  END FUNCTION

  END
