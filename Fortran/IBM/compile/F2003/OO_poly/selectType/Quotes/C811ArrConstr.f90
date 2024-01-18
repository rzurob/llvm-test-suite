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
! %POSTCMD: tcomp C811ArrConstr.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C811ArrConstr
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
!*    The selector is an array constructor without ssociate-name =>
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C811ArrConstr
  IMPLICIT NONE

  SELECT TYPE ( (/1, 2, 3/))
    TYPE IS (INTEGER)
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  END

