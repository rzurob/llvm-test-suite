! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 26, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  Module procedure and external procedure
!*  (314738)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  CONTAINS
    FUNCTION ExtFun(Arg)
    CLASS(*), POINTER :: ExtFun
    CLASS(*)          :: Arg
      ALLOCATE(ExtFun, SOURCE=Arg)
    END FUNCTION
  END MODULE

  FUNCTION ExtFun(Arg)
  CLASS(*), POINTER :: ExtFun
  CLASS(*) ::  Arg
    ALLOCATE(ExtFun, SOURCE=Arg)
  END FUNCTION

  PROGRAM PtrAssignMisc3
  USE M, ModFun => ExtFun
  IMPLICIT NONE

  PROCEDURE(ModFun),   POINTER :: ProcPtr1
  PROCEDURE(ProcPtr1), POINTER :: ProcPtr2

  INTERFACE
    FUNCTION ExtFun(Arg)
      CLASS(*), POINTER :: ExtFun
      CLASS(*)          :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(ExtFun),   POINTER :: ProcPtr3
  PROCEDURE(ProcPtr3), POINTER :: ProcPtr4

  ProcPtr1 => ModFun
  SELECT TYPE ( As => ProcPtr1(-1_1))
  TYPE IS (INTEGER(1))
    IF ( As .NE. -1 ) STOP 11
  CLASS DEFAULT
    STOP 12
  END SELECT

  ProcPtr2 => ExtFun
  SELECT TYPE ( As => ProcPtr2(1.0_8))
  TYPE IS (REAL(8))
    IF ( As .NE. 1.0_8 ) STOP 21
  CLASS DEFAULT
    STOP 22
  END SELECT

  ProcPtr3 => ModFun
  SELECT TYPE ( As => ProcPtr3((1.0, -1.0)) )
  TYPE IS (COMPLEX)
    IF ( As .NE. (1.0, -1.0) ) STOP 31
  CLASS DEFAULT
    STOP 32
  END SELECT

  ProcPtr4 => ExtFun
  SELECT TYPE ( As => ProcPtr4("1234567890"))
  TYPE IS (CHARACTER(*))
    IF ( As .NE. "1234567890" ) STOP 41
  CLASS DEFAULT
    STOP 42
  END SELECT


  END

