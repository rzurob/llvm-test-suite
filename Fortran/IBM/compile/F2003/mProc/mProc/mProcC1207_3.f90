!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcC1207_3.f
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
!*  (316778)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  FUNCTION IFun(Arg)
  INTEGER :: Arg, IFun
    IFun =  Arg
  END FUNCTION

  PROGRAM mProcC1207_3

  ABSTRACT INTERFACE
    FUNCTION IFun(Arg)
     INTEGER :: Arg, IFun
    END FUNCTION
  END INTERFACE

  INTERFACE Fun
    PROCEDURE IFun
  END INTERFACE

  !PRINT*, Fun(1) ! funny it works here :)

  END


