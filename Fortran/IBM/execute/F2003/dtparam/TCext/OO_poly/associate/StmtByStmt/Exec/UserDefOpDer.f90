! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/UserDefOpDer.f
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
!*    The selector is an expression of derived type with user defened operator
!*    (Comp Failed )
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

  PROGRAM UserDefOpDer
  USE M

  INTERFACE OPERATOR ( .OP. )
    FUNCTION MyOp (Arg1, Arg2)
      IMPORT Base, Child
      TYPE(Base(4)),  INTENT(IN) :: Arg1
      TYPE(Child(4)), INTENT(IN) :: Arg2
      TYPE(Child(4))             :: MyOp
    END FUNCTION
  END INTERFACE OPERATOR ( .OP. )

  ASSOCIATE ( As => Base(4)() .OP. Child(4)() )
    IF( As%BaseId       .NE. 2 ) STOP 46
    IF( As%Base%GetId() .NE. 2 ) STOP 47
    IF( As%GetId()      .NE. 3 ) STOP 48
    IF( As%ChildId      .NE. 3 ) STOP 49
  END ASSOCIATE

  END

  FUNCTION MyOp (Arg1, Arg2)
  USE M

    TYPE(Base(4)),  INTENT(IN) :: Arg1
    TYPE(Child(4)), INTENT(IN) :: Arg2
    TYPE(Child(4))             :: MyOp

    MyOp%BaseId  = Arg1%BaseId  + Arg2%BaseId
    MyOp%ChildId = Arg1%BaseId  + Arg2%ChildId
  END FUNCTION

