! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 08, 2005
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
!*  Procedure pointer can not be in IO
!*
!*  (304914)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc10
  IMPLICIT INTEGER(P)

  PROCEDURE(),        POINTER :: ProcPtr1 => NULL()
  PROCEDURE(INTEGER), POINTER :: ProcPtr2 => NULL()


  READ *, ProcPtr1
  READ *, ProcPtr2

  PRINT *,ProcPtr1
  PRINT *,ProcPtr2


  END


