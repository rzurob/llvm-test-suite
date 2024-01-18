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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  CONTAINS
    FUNCTION ExtFun()
    CHARACTER(3), POINTER :: ExtFun
      !ALLOCATE(ExtFun, SOURCE="mod")
      ALLOCATE(ExtFun)
      ExtFun = "mod"
    END FUNCTION
  END MODULE

  FUNCTION ExtFun()
  CHARACTER(3), POINTER :: ExtFun
    !ALLOCATE(ExtFun, SOURCE="ext")
    ALLOCATE(ExtFun)
    ExtFun = "ext"
  END FUNCTION

  PROGRAM PtrAssignMisc3
  USE M, ModFun => ExtFun
  IMPLICIT NONE

  PROCEDURE(ModFun),   POINTER :: ProcPtr1
  PROCEDURE(ProcPtr1), POINTER :: ProcPtr2

  INTERFACE
    FUNCTION ExtFun()
      CHARACTER(3), POINTER :: ExtFun
    END FUNCTION
  END INTERFACE

  PROCEDURE(ExtFun),   POINTER :: ProcPtr3
  PROCEDURE(ProcPtr3), POINTER :: ProcPtr4

  ProcPtr1 => ModFun
    IF ( ProcPtr1() .NE. "mod" )  ERROR STOP 11

  ProcPtr2 => ExtFun
    IF (  ProcPtr2() .NE. "ext" ) ERROR STOP 21

  ProcPtr3 => ModFun
    IF ( ProcPtr3() .NE. "mod" )  ERROR STOP 31

  ProcPtr4 => ExtFun
    IF ( ProcPtr4() .NE. "ext" )  ERROR STOP 41


  END

