!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypParamC435
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
!* 435 (R435) Each type-param-name in the derived-type-stmt in a derived-type-def
!* shall appear as a  type-param-name in a type-param-def-stmt in that
!* derived-type-def.
!*
!*  (wrong err msg)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypParamC435

  TYPE :: DT0(K, L)
    INTEGER, KIND :: K=0
    INTEGER, LEN  :: L=0
    INTEGER       :: I
  END TYPE

  TYPE :: DT01
    INTEGER, KIND :: K=0
    INTEGER, LEN  :: L=0
  END TYPE

  TYPE :: DT1(K, L)
    INTEGER(2) :: K=0
    INTEGER(4) :: L=0
  END TYPE

  TYPE :: DT2(L)
    INTEGER(4), LEN :: L=0
    INTEGER(4) :: L=0
  END TYPE

  TYPE, EXTENDS(DT0) :: DT3(K)
  END TYPE

  TYPE, EXTENDS(DT0) :: DT4(L)
  END TYPE

  TYPE, EXTENDS(DT0) :: DT5(I)
  END TYPE

  TYPE, EXTENDS(DT0) :: DT6(KK, LL)
    INTEGER, KIND :: KK(3)=0
    INTEGER, LEN  :: LL(1)=0
  END TYPE

  END


