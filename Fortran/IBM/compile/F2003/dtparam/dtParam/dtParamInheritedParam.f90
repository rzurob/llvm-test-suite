!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dtParamInheritedParam  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 22, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type parameters 
!*
!*  REFERENCE                  : Feature Number 289057
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
!*  Inherited parameters 
!*  
!*  (Passing)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamInheritedParam 
  IMPLICIT NONE

  TYPE :: DT(K)
    INTEGER, KIND :: K=1
  END TYPE

  TYPE, EXTENDS(DT) :: DT1(K)
    INTEGER, KIND :: K=0
  END TYPE 

  TYPE :: DT2(L)
    INTEGER, LEN :: L=1
  END TYPE

  TYPE, EXTENDS(DT2) :: DT3(L)
    INTEGER, LEN :: L=0
  END TYPE

  TYPE :: DT4(L)
    INTEGER, LEN :: L=1
  END TYPE

  TYPE, EXTENDS(DT2) :: DT5(L)
    INTEGER, KIND :: L=0
  END TYPE

  END


