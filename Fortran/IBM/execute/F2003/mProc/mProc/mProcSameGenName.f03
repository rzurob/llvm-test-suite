!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 14, 2006
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
!* If a generic invocation applies to both a specific procedure from an interface and an accessible
!* generic intrinsic procedure, it is the specific procedure from the interface that is referenced.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  FUNCTION ModFun(Arg)
  REAL :: ModFun, Arg
    ModFun = Arg
  END FUNCTION

  FUNCTION ModFun1(Arg)
  REAL :: ModFun1, Arg
    ModFun1 = Arg
  END FUNCTION

  END MODULE

  PROGRAM mProcSameGenName
  USE M

  INTERFACE Mod
    PROCEDURE ModFun
  END INTERFACE

  INTERFACE Abs
    PROCEDURE ModFun1
  END INTERFACE

  IF ( Mod(1.1)  .NE. 1.1   )               ERROR STOP 21

  IF ( ABS(-1.0)  .NE. -1.0   )             ERROR STOP 22

  END



