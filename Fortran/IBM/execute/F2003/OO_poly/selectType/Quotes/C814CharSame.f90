! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  C814CharSame.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C814CharSame
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C814
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
!*    The selector is a char, and test the len param
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C814CharSame
  IMPLICIT NONE

  CALL Sub("1234")

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg

  SELECT TYPE ( Arg )
    TYPE IS (CHARACTER(*))
      PRINT*, SIZEOF(Arg)
      IF (SIZEOF(Arg) .NE. 4 ) STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT

  END SUBROUTINE

  END

