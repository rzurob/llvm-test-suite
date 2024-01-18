! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  C809AssoName7.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C809AssoName7
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Associate name
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
!*     The associate name must only be declared once in the ASSOCIATE statement
!*     Selector is an array section with subscript which has the same name
!*     as associate name
!*    (ICE)
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: Id = 0
    CONTAINS
      PROCEDURE, NOPASS :: PrintType => PrintChild
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    SUBROUTINE PrintChild()
      PRINT *,'Child'
    END SUBROUTINE

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%Id
    END FUNCTION

  END MODULE

  PROGRAM C809AssoName7
  USE M
  IMPLICIT NONE
  INTEGER      :: I
  TYPE (Child) :: V(3) = (/ (Child(i), i = 1, 3) /)

    ASSOCIATE ( V => V((/1,2,3/)) )
      IF ( ANY(SHAPE(V)  .NE. (/3/)))      STOP 50
      IF ( ANY(V%GetId() .NE. (/1,2,3/)))  STOP 51

      ASSOCIATE ( V => V((/2,2/))  )
        IF ( ANY(SHAPE(V)  .NE. (/2/)))   STOP 52
        IF ( ANY(V%Id      .NE. (/2,2/))) STOP 53
        IF ( ANY(V%GetId() .NE. (/2,2/))) STOP 54
      END ASSOCIATE
    END ASSOCIATE

  END

