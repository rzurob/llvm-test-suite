!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcSyntax.f
!*
!*  DATE                       : Feb. 27, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Generaliztion of PROCEDURE statement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 296676
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Syntax
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS
  CHARACTER(3) FUNCTION ModFun(Arg)
  INTEGER :: Arg
    ModFun = "OK0"
  END FUNCTION

  END MODULE

  CHARACTER(3) FUNCTION ExtFun1(Arg)
  COmPlex Arg
    ExtFun1 = "OK1"
  END FUNCTION

  CHARACTER(3) FUNCTION ExtFun2(Arg)
  INTEGER(1)  Arg
    ExtFun2 = "OK2"
  END FUNCTION

  CHARACTER(3) FUNCTION ExtFun3(Arg)
  LOGICAL  Arg
    ExtFun3 = "OK3"
  END FUNCTION

  PROGRAM mProcSyntax
  USE M
  IMPLICIT NONE

  INTERFACE
    CHARACTER(3) FUNCTION ExtFun3(Arg)
      LOGICAL   Arg
    END FUNCTION
    CHARACTER(3) FUNCTION ExtFun2(Arg)
      COmPlex Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(ExtFun3), POINTER ::  ProcPtr


  CALL Check(ExtFun2)
  CONTAINS

  SUBROUTINE Check(Proc)

  INTERFACE Fun
    CHARACTER(3) FUNCTION ExtFun1(Arg)
      REAL Arg
    END FUNCTION
    CHARACTER(3) FUNCTION Proc(Arg)
      INTEGER(1)  Arg
    END FUNCTION
    PROCEDURE ModFun, ExtFun1, Proc, ProcPtr
  END INTERFACE

  ProcPtr => ExtFun3

  IF (Fun(1)       .NE. "OK0") STOP 11
  IF (Fun(1.)      .NE. "OK1") STOP 12
  IF (Fun(1_1)     .NE. "OK2") STOP 13
  IF (Fun(.TRUE.)  .NE. "OK3") STOP 14


  END SUBROUTINE

  END


