! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ElemFuncRetPolyDer.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ElemFuncRetPolyDer
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
!*    The selector is an elemental func call returning a poly var
!*    of derived type
!*    ( Comp Failed)
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

  PROGRAM ElemFuncRetPolyDer
  USE M
  Type(Child) :: V(2, 2)

  ASSOCIATE ( As => Func( V ) )
    IF ( .NOT. ALL ( SHAPE(As) .EQ. (/2,2/)) ) STOP 81
    IF( ANY(As%BaseId       .NE. -1) ) STOP 46
    IF( ANY(As%Base%GetId() .NE. -1) ) STOP 47
    IF( ANY(As%GetId()      .NE. -2) ) STOP 48
    IF( ANY(As%ChildId      .NE. -2) ) STOP 49
  END ASSOCIATE

  CONTAINS

  ELEMENTAL FUNCTION Func(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    TYPE(Child)              :: Func

    ASSOCIATE ( As => Arg)
      SELECT TYPE (As )
      TYPE IS (Child)
        Func = Child(BaseId=-1, ChildId=-2)
      END SELECT
    END ASSOCIATE
  END FUNCTION

  END
