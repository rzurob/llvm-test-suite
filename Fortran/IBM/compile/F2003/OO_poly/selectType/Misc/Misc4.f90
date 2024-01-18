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
! %POSTCMD: tcomp Misc4.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc4
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*   No poly entity as selector
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  Misc4

  SELECT TYPE ( As => Pt )
  TYPE IS (CHARACTER(*))
    IF ( As .NE. "1" )  STOP 3
  END SELECT

  END

