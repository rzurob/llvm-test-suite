!*********************************************************************
!*  ===================================================================
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

  INTERFACE Fun

    PROCEDURE ModFun

    CHARACTER(3) FUNCTION ExtFun(Arg)
    IMPORT
      REAL Arg
      INTERFACE Fun1
        PROCEDURE ModFun,ModFun1
      END INTERFACE
    END FUNCTION
    PROCEDURE   ExtFun

  END INTERFACE

  CONTAINS

  CHARACTER(3) FUNCTION ModFun(Arg)
  INTEGER :: Arg
    ModFun = "OK0"
  END FUNCTION

  CHARACTER(3) FUNCTION ModFun1(Arg)
  INTEGER(1) :: Arg
  INTERFACE Fun1
    PROCEDURE ModFun
  END INTERFACE

    ModFun1 = "OK1"
  END FUNCTION

  END MODULE

  CHARACTER(3) FUNCTION ExtFun(Arg)
  REAL Arg
    ExtFun = "OK!"
  END FUNCTION

  PROGRAM mProcSyntax1
  USE M, ONLY : Fun, ModFun1

  INTERFACE Fun
    PROCEDURE   ModFun1
  END INTERFACE

  IF (Fun(1.0) .NE. "OK!") ERROR STOP 11
  IF (Fun(1)   .NE. "OK0") ERROR STOP 12
  IF (Fun(1_1) .NE. "OK1") ERROR STOP 13


  END


