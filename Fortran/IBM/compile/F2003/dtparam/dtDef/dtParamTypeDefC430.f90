!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefC430 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 30, 2005
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type definition 
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
!*  C430 (R429) The same private-or-sequence shall not appear more than
!*  once in a given derived-type-def .
!*
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M 

    TYPE :: DT0(K0)
      INTEGER, KIND :: K0=0
      PRIVATE
    END TYPE

    TYPE, EXTENDS(DT0) :: DT3(K, L)
      INTEGER, KIND :: K=0
      INTEGER, LEN  :: L=0
      PRIVATE
    END TYPE

    TYPE :: DT4(K, L)
      INTEGER, KIND :: K=0
      INTEGER, LEN  :: L=0
      PRIVATE
      SEQUENCE 
      PRIVATE
    END TYPE

    TYPE :: DT5(K, L)
      INTEGER, KIND :: K=0
      INTEGER, LEN  :: L=0
      SEQUENCE 
      PRIVATE
      SEQUENCE 
      PRIVATE
    END TYPE

    TYPE :: DT6(K, L)
      INTEGER, KIND :: K=0
      INTEGER, LEN  :: L=0
      PRIVATE 
      SEQUENCE 
      SEQUENCE 
      SEQUENCE 
    END TYPE

  END MODULE 
    
  PROGRAM dtParamTypeDefC430
  
  END

