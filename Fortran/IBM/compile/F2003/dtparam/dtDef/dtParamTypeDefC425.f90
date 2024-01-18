!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefC425 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 29, 2005
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Syntax
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
!*  C425 (R430) The same type-attr-spec shall not appear more than once 
!*  in a given derived-type-stmt. 
!*
!*  (No complaint on DT3)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE DT
  END TYPE

  TYPE, BIND(C), PUBLIC, BIND(C)  :: DT1(K)
    INTEGER, KIND :: K
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT), ABSTRACT  :: DT3(K) ! no complaint here
    INTEGER, KIND :: K
  END TYPE

  TYPE, EXTENDS(DT), ABSTRACT, EXTENDS(DT)  :: DT4(K)  
    INTEGER, KIND :: K
  END TYPE

  TYPE, PUBLIC, EXTENDS(DT), PUBLIC   :: DT5(K)
    INTEGER, KIND :: K
  END TYPE

  TYPE,  PUBLIC, PRIVATE  :: DT2(K)
    INTEGER, KIND :: K
  END TYPE


  END MODULE

  PROGRAM dtParamTypeDefC425 
  
  END

