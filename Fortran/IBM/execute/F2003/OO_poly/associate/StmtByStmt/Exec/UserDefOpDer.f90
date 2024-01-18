! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  UserDefOpDer.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : UserDefOpDer
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

    TYPE :: Base
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

  PROGRAM UserDefOpDer
  USE M

  INTERFACE OPERATOR ( .OP. )
    FUNCTION MyOp (Arg1, Arg2)
      IMPORT Base, Child
      TYPE(Base),  INTENT(IN) :: Arg1
      TYPE(Child), INTENT(IN) :: Arg2
      TYPE(Child)             :: MyOp
    END FUNCTION
  END INTERFACE OPERATOR ( .OP. )

  ASSOCIATE ( As => Base() .OP. Child() )
    IF( As%BaseId       .NE. 2 ) STOP 46
    IF( As%Base%GetId() .NE. 2 ) STOP 47
    IF( As%GetId()      .NE. 3 ) STOP 48
    IF( As%ChildId      .NE. 3 ) STOP 49
  END ASSOCIATE

  END

  FUNCTION MyOp (Arg1, Arg2)
  USE M

    TYPE(Base),  INTENT(IN) :: Arg1
    TYPE(Child), INTENT(IN) :: Arg2
    TYPE(Child)             :: MyOp

    MyOp%BaseId  = Arg1%BaseId  + Arg2%BaseId
    MyOp%ChildId = Arg1%BaseId  + Arg2%ChildId
  END FUNCTION

