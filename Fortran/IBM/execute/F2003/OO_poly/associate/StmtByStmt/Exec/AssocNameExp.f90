! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  AssocNameExp.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : AssocNameExp
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
!*    The selector is expressed by associating entities
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS
    FUNCTION GetChildId(Arg)
    CLASS(Child) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

  END MODULE

  PROGRAM AssocNameExp
  USE M
  IMPLICIT TYPE(Child)(A, U)

  DIMENSION U(3)

  ASSOCIATE ( As => U  )
    U1%BaseId  = -3
    U1%ChildId = -1

    ASSOCIATE ( As1 => U1 )
      ASSOCIATE ( As2 => As%ChildID + As1%BaseId )
        IF ( ANY(As2 .NE. (/-1, -1, -1/))) STOP  30
      END ASSOCIATE
    END ASSOCIATE

  END ASSOCIATE

  END
