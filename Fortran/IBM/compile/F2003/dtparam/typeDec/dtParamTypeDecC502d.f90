!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC502d
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 24, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
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
!*  
!* C502 (R502) In a declaration-type-spec that uses the CLASS keyword, 
!* derived-type-spec shall specify an extensible type 
!*
!*     
!*
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC502d

  TYPE :: DT(K)
    INTEGER, KIND :: K=4
    SEQUENCE
  END TYPE

  CLASS(DT(K=4)), ALLOCATABLE  :: T
 
  CONTAINS

  SUBROUTINE IntSub(Arg)

  TYPE :: DT1(L)
    INTEGER, LEN  :: L=1
    SEQUENCE
  END TYPE

  CLASS(DT1(L=1))  :: Arg

  END SUBROUTINE

  END

