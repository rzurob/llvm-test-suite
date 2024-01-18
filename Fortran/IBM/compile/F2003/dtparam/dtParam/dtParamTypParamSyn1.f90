!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypParamDef1   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 15, 2005
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
!*  Type param def stmt - Syntax 
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  TYPE :: DT1(K)
    INTEGER(KIND=1)  KIND :: K=0
  END TYPE

  TYPE :: DT2(L)
    INTEGER(KIND=2),  :: L=0
  END TYPE

  TYPE :: DT3(L)
    INTEGER(KIND= ),LEN :: L=0
  END TYPE

  TYPE :: DT4(K)
    INTEGER(NotKind=4), KIND :: K=0 
  END TYPE

  TYPE :: DT5(K)
    INTEGER(Kind=4), KIND :: K=0, K
  END TYPE

  TYPE :: DT6(L)
    INTEGER(Kind=4), LEN :: L, L
  END TYPE

  TYPE :: DT7(K)
    INTEGER(KIND=4), UNKNOWN :: K
  END TYPE

  
  END


