!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypParamC434
!*
!*  DATE                       : Jan. 26, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type parameters
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  C434 (R435) A type-param-name in a type-param-def-stmt in a derived-type-def
!*  shall be one of the  type-param-names in the derived-type-stmt of that
!*  derived-type-def
!*
!*  (err msg missing except for line 81 & ice)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypParamC435

  TYPE :: DT0(K, L)
    INTEGER, KIND :: K=0
    INTEGER, LEN  :: L=0
    INTEGER       :: I
  END TYPE

  TYPE :: DT1(K)
    INTEGER(2), KIND :: L=0
  END TYPE

  TYPE :: DT2(L)
    INTEGER(4), LEN :: K=0
  END TYPE

  TYPE, EXTENDS(DT0) :: DT3
    INTEGER, KIND :: L=0
  END TYPE

  TYPE, EXTENDS(DT0) :: DT4
    INTEGER, KIND :: K=0
  END TYPE

  TYPE, EXTENDS(DT0) :: DT5
    INTEGER, KIND :: I=0
  END TYPE

  TYPE, EXTENDS(DT0) :: DT6(K, L)  ! Dup of parent
    INTEGER, KIND :: K=0
    INTEGER, LEN  :: L=0
  END TYPE

  TYPE :: DT7(K)
    INTEGER(2), KIND :: K=0
    INTEGER(2)  :: K
  END TYPE

  TYPE :: DT8
    INTEGER(2), KIND :: K=0
    INTEGER(2)  :: K
  END TYPE

  TYPE :: DT9()
  END TYPE



  END


