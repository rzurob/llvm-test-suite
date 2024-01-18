!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefC1502 
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
!*  C1502 (R429) A derived type with the BIND attribute shall not have type parameters. 
!*
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefC1502 
  USE ISO_C_BINDING

  TYPE, BIND(C) :: DT0(K)
    INTEGER, KIND :: K
  END TYPE

  TYPE, BIND(C) :: DT1(L)
    INTEGER, LEN :: L
  END TYPE

  TYPE, BIND(C) :: DT2(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    INTEGER(C_INT):: I
    REAL(C_FLOAT) :: R    
  END TYPE


  ! The following is fine
  TYPE, BIND(C) :: DT3
    INTEGER(C_INT):: I
    REAL(C_FLOAT) :: R    
  END TYPE

  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    TYPE(DT3)     :: B 
    REAL(K) :: R
  END TYPE

  END

