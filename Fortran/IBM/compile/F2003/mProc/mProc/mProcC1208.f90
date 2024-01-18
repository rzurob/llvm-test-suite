!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcC1208.f  
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
!*  C1208 (R1206) If MODULE appears in a procedure-stmt, each procedure-name
!*  in that statement shall be accessible in the current scope as a module 
!*  procedure. 
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  PROCEDURE(ModFun1), POINTER :: ProcPtr

  INTERFACE Fun
    MODULE PROCEDURE ModFun, ProcPtr
  END INTERFACE

  PROCEDURE(ModFun) :: Proc
  INTERFACE Fun1
    MODULE PROCEDURE ModFun1, Proc 
    MODULE PROCEDURE test 
  END INTERFACE

  CONTAINS

  CHARACTER(3) FUNCTION ModFun(Arg)
  INTEGER :: Arg   
    ModFun = "OK0"
  END FUNCTION

  FUNCTION ModFun1(Arg)
  INTEGER(1) :: Arg 
  CHARACTER(3) :: ModFun1, Test, Arg1
 
  ENTRY test(arg1)
    ModFun1 = "OK1"
  END FUNCTION

  END MODULE

  PROGRAM mProcC1208 
  USE M
  
  INTERFACE 
    CHARACTER(3) FUNCTION ExtFun(Arg)
      INTEGER(1) :: Arg
    END FUNCTION
  END INTERFACE

  INTERFACE Fun2 
    MODULE PROCEDURE ModFun,  ExtFun 
  END INTERFACE


  END


