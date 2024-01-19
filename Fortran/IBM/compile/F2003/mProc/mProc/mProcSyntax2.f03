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


  CONTAINS

  CHARACTER(3) FUNCTION ModFun(Arg)
  INTEGER :: Arg
    ModFun = "OK0"
  END FUNCTION

  END MODULE

  PROGRAM mProcSyntax2
  USE M

  INTERFACE Fun
    PROCEDURE
  END INTERFACE

  INTERFACE Fun
    PROCEDURE ModFun ModFun
  END INTERFACE

  INTERFACE Fun
    PROCEDURE(ModFun)  ModFun
  END INTERFACE

  INTERFACE Fun
    PROCEDURE()  Sub
  END INTERFACE

  INTERFACE Fun
    PROCEDURE ModFun, ModFun  !C1209 will be removed
  END INTERFACE


  END


