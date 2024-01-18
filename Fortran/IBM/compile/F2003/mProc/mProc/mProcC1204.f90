!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcC1204.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 27, 2006
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
!*  C1204 (R1202) A procedure-stmt is allowed only in an interface
!*  block that has a generic-spec. 
!*
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

  PROGRAM mProcC1204 
  USE M 

  INTERFACE 
    PROCEDURE  ModFun
  END INTERFACE

  ABSTRACT INTERFACE 
    PROCEDURE  ModFun
  END INTERFACE

  END


