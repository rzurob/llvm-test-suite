!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcC1209.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 01, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Generaliztion of PROCEDURE statement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 296676 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  C1209 (R1206) A procedure-name shall not specify a procedure that is 
!*  specified previously in any procedure-stmt in any accessible interface with
!*  the same generic identifier. 
!*  This one is removed by feature 296275 -- we can test the opposite
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


  INTERFACE Fun
    PROCEDURE ModFun
    PROCEDURE ModFun
  END INTERFACE


  CONTAINS

  FUNCTION ModFun(Arg)
  INTEGER :: Arg, ModFun   
    ModFun = Arg 
  END FUNCTION


  END MODULE

  FUNCTION ExtProc(Arg)
  INTEGER :: Arg, ExtProc   
    ExtProc = Arg 
  END FUNCTION

  PROGRAM mProcC1209 
  USE M

  PROCEDURE(ModFun), POINTER :: ProcPtr
  PROCEDURE(ModFun)          :: ExtProc

  INTERFACE Fun1
    PROCEDURE ProcPtr 
    PROCEDURE ProcPtr 
  END INTERFACE

  INTERFACE Fun2
    PROCEDURE ExtProc 
    PROCEDURE ExtProc 
  END INTERFACE

  IF (Fun(1)    .NE. 1 ) STOP 11

  ProcPtr => ModFun
  IF (Fun1(2)   .NE. 2 ) STOP 12

  IF (Fun2(3)   .NE. 3 ) STOP 13

  CALL IntSub(ExtProc)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ModFun)  Proc

  INTERFACE Fun3
    PROCEDURE Proc
    PROCEDURE Proc
  END INTERFACE

  IF (Fun3(4)   .NE. 4 ) STOP 14

  END SUBROUTINE

  END


