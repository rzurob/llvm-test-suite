! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/FuncArgDer.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  FuncArgDer.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncArgDer
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

  PROGRAM FuncArgDer
  USE M
  TYPE (Child(4)) :: V

  ASSOCIATE ( As1 => V )
  ASSOCIATE ( As => As1 )
    CALL Sub(As, As1)
    IF ( V%GetID()      .NE. -2) STOP 50
    IF ( V%ChildId      .NE. -2) STOP 51
    IF ( V%Base%GetID() .NE. -1) STOP 52
    IF ( V%BaseId       .NE. -1) STOP 53
  END ASSOCIATE
  END ASSOCIATE

  CONTAINS

  SUBROUTINE Sub(Arg, Arg1)
    TYPE(Child(4)) :: Arg, Arg1
    TYPE(Child(4))  :: Func

    IF (Arg%GetId()      .NE. 2 ) STOP 20
    IF (Arg%Base%GetId() .NE. 1 ) STOP 21

    Arg1%BaseId = -1
    Arg1%ChildId = -2

  END SUBROUTINE

  END
