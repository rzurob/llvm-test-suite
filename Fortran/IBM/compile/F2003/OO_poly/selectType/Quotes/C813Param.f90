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
! %POSTCMD: tcomp C813Param.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C813Param
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C813
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
!*    The selector is a parameter  array of char
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C813Param
  IMPLICIT NONE

  CHARACTER(LEN=3), PARAMETER :: ArrChar(4) = "321"

  SELECT TYPE (  As => ArrChar )
    TYPE IS (INTEGER(2))
      STOP 50
    CLASS DEFAULT
      STOP 30
    TYPE IS (CHARACTER(*))
      STOP 20
  END SELECT
  STOP 40

  END

