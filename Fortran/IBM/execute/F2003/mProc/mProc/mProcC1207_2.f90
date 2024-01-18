!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Generalization of PROCEDURE statement
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
!*  C1207 (R1206) A procedure-name shall have an explicit interface and shall
!*  refer to an accessible procedure pointer, external procedure,
!*  dummy procedure, or module procedure.
!*
!*  (316777/317589)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  FUNCTION IFun(Arg)
  INTEGER :: Arg, IFun
    IFun =  Arg
  END FUNCTION

  MODULE M0
  INTERFACE Fun
    FUNCTION IFun(Arg)
     INTEGER :: Arg, IFun
    END FUNCTION
    PROCEDURE IFun
  END INTERFACE

  PRIVATE IFun

  END MODULE

  MODULE M1

  INTERFACE Fun
    PROCEDURE IFun
  END INTERFACE

  INTERFACE Fun
    FUNCTION IFun(Arg)
      INTEGER :: Arg, IFun
    END FUNCTION
  END INTERFACE

  END MODULE

  MODULE M2

  INTERFACE Fun
    PROCEDURE ProcPTr
  END INTERFACE

  PROCEDURE(IFun), POINTER :: ProcPtr

  INTERFACE
    FUNCTION IFun(Arg)
      INTEGER :: Arg, IFun
    END FUNCTION
  END INTERFACE

  END MODULE

  MODULE M3
   USE M0
  END MODULE

  PROGRAM mProcC1207_2
  USE M0, ONLY: Fun0 => Fun
  USE M1, ONLY: Fun1 => Fun
  USE M2, ONLY: Fun2 => Fun, Ptr => ProcPtr, ExtFun => IFun
  USE M3, ONLY: Fun3 => Fun

  IF (Fun0(0) .NE. 0 ) ERROR STOP 10
  IF (Fun1(1) .NE. 1 ) ERROR STOP 11

  Ptr => ExtFun
  IF (Fun2(2) .NE. 2 ) ERROR STOP 12
  IF (Fun3(3) .NE. 3 ) ERROR STOP 13


  END


