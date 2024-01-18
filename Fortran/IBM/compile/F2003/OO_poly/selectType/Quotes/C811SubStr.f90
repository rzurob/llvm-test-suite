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
! %POSTCMD: tcomp C811SubStr.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C811SubStr
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
!*    The selector is a substring without assciate name.
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C811SubStr
  IMPLICIT NONE

  CHARACTER(10) :: Str = "1234567890"

  SELECT TYPE (  Str(2:1) )
    TYPE IS (CHARACTER(*))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  END

