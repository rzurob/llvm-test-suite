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
! %POSTCMD: tcomp C811NonDataInterface.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C811NonDataInterface
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C811
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is an interface name.
!*    (Diagnostic)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C811NonDataInterface
  IMPLICIT NONE

  INTERFACE NonData
    SUBROUTINE Sub()
    END Subroutine
  END INTERFACE

  SELECT TYPE ( As => NonData() )
    TYPE IS (CHARACTER(*))
      STOP 20
    TYPE IS (INTEGER(2))
      STOP 50
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  END

