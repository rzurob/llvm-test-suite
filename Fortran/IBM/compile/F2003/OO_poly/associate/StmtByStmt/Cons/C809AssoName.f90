! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp C809AssoName.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C809AssoName
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
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: Id = 0
    CONTAINS
      PROCEDURE, NOPASS :: PrintType => PrintChild
    END TYPE

  CONTAINS

    SUBROUTINE PrintChild()
      PRINT *,'Child'
    END SUBROUTINE

  END MODULE

  PROGRAM C809AssoName
  USE M
  IMPLICIT NONE
  TYPE(Child) :: V

    ASSOCIATE ( As => Child(), As => V  )
    END ASSOCIATE

  END
