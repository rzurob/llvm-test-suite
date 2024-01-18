! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp C815TypeIsSeq.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C815TypeIsSeq
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C815
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
!*   Use sequence type as type spec
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C815TypeIsSeq
  IMPLICIT NONE

  TYPE :: Seq
    SEQUENCE
    INTEGER :: i
  END TYPE

  TYPE(Seq) :: Arg

  CALL Sub(Arg)

  CONTAINS

  SUBROUTINE Sub(Arg)

  CLASS(*) :: Arg

  SELECT TYPE ( Arg )
    TYPE IS (Seq)
    CLASS IS (Seq)
    CLASS DEFAULT
      STOP 30
  END SELECT

  END SUBROUTINE
  END

