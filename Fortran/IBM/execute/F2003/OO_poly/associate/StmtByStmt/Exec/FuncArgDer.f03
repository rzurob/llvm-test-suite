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
!*    The associating entity of derived type is used as actual argument
!*    ()
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
      CLASS(Child),  POINTER :: ChildComp  => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM FuncArgDer
  USE M
  TYPE (Child) :: V

  ASSOCIATE ( As1 => V )
  ASSOCIATE ( As => As1 )
    CALL Sub(As, As1)
    IF ( V%GetID()      .NE. -2) ERROR STOP 50
    IF ( V%ChildId      .NE. -2) ERROR STOP 51
    IF ( V%Base%GetID() .NE. -1) ERROR STOP 52
    IF ( V%BaseId       .NE. -1) ERROR STOP 53
  END ASSOCIATE
  END ASSOCIATE

  CONTAINS

  SUBROUTINE Sub(Arg, Arg1)
    TYPE(Child) :: Arg, Arg1
    TYPE(Child)  :: Func

    IF (Arg%GetId()      .NE. 2 ) ERROR STOP 20
    IF (Arg%Base%GetId() .NE. 1 ) ERROR STOP 21

    Arg1%BaseId = -1
    Arg1%ChildId = -2

  END SUBROUTINE

  END
