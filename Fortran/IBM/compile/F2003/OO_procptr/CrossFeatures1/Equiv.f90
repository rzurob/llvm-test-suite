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
! %POSTCMD: tcomp Equiv.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Equivalence.f
!*
!*  DATE                       : May. 29, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Equivalence
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Equiv

  PROCEDURE(INTEGER), POINTER :: ProcPtr1, ProcPtr2
  PROCEDURE(),        POINTER :: ProcPtr3, ProcPtr4

  COMMON ProcPtr1, ProcPtr2

  EQUIVALENCE  (ProcPtr1, ProcPtr3)
  EQUIVALENCE  (ProcPtr2, ProcPtr4)

  END

