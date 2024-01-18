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
! %POSTCMD: tcomp C811ArrConstr2.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C811ArrConstr2
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
!*    (Wrong semantic check)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C811ArrConstr2
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr(:)

  ALLOCATE(Ptr(3), SOURCE=(/1_2, 2_2, 3_2/) )

  SELECT TYPE ( (/Ptr/) )
    TYPE IS (INTEGER(2))
      STOP 10
    TYPE IS (INTEGER(1))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  END

