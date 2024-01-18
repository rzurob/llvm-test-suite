!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefC424 
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
!*  C424 (R430) A derived type type-name shall not be DOUBLEPRECISION or the same as
!*  the name of any intrinsic type defined in this standard. 
!*
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefC424 

  TYPE, BIND(C) :: DOUBLEPRECISION(K)
    INTEGER, KIND :: K
  END TYPE

  TYPE  INTEGER(K)
    INTEGER, LEN :: K
  END TYPE

  TYPE, ABSTRACT :: REAL(K)
    INTEGER, LEN :: K=6
  END TYPE

  TYPE  :: CHARACTER(K)
    INTEGER, LEN :: K
    CHARACTER(K) :: C
  END TYPE

  TYPE :: LOGICAL(K)
    INTEGER, KIND :: K
    LOGICAL(K) :: L
  END TYPE

  TYPE :: COMPLEX(K1, K2, K3)
    INTEGER, KIND :: K1
    INTEGER, KIND :: K2
    INTEGER, KIND :: K3
  END TYPE

  
  END

