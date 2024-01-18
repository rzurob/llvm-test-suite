! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  VarImplicit.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : VarImplicit
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
!*    The selector is a non-poly variable of implied type
!*    (Comp failed)
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
      TYPE(Base) :: BaseComp = Base(0)
    CONTAINS
      PROCEDURE, NOPASS :: PrintType => PrintChild
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    SUBROUTINE PrintChild()
      PRINT *,'Child'
    END SUBROUTINE

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

  PROGRAM VarImplicit
  USE M
  IMPLICIT TYPE(Child)(A, U)

  ASSOCIATE ( As => U )
    IF ( As%GetID() .NE. 2) STOP 50

    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( As0 .NE. 2) STOP 51
       IF ( As1 .NE. 1) STOP 52
    END ASSOCIATE

    ASSOCIATE ( As2 => As%Base )
      IF ( As2%GetID() .NE. 1 ) STOP 53
    END ASSOCIATE

    ASSOCIATE (As => As%GetID())
      IF ( As .NE. 2 ) STOP 60
    END ASSOCIATE

    ASSOCIATE (As =>  As%Base%GetID())
      IF ( As .NE. 1 ) STOP 61
    END ASSOCIATE

    ASSOCIATE (As =>  As%BaseComp%GetID())
      IF ( As .NE. 0 ) STOP 62
    END ASSOCIATE

  END ASSOCIATE

  END
