!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypParamR437
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
!*  Diag: R437 type-param-attr-spec is KIND or LEN
!*
!*  (no complaint on DT5(6/7/8))
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypParamR437

  TYPE :: DT0(K, L)
    INTEGER, KIND :: K=0
    INTEGER, LEN  :: L=0
    INTEGER       :: I
  END TYPE

  TYPE :: DT01
    INTEGER :: K=0
    INTEGER :: L=0
  END TYPE

  TYPE :: DT1(K)
    INTEGER, XXX :: K=0
  END TYPE

  TYPE :: DT2(L)
    INTEGER(4), LEN, LEN :: L=0
  END TYPE

  TYPE :: DT3(K)
    INTEGER(4), KIND, KIND :: K=0
  END TYPE

  TYPE :: DT4(K)
    INTEGER, KIND, LEN :: K=0
  END TYPE

  TYPE :: DT6(KIND, LEN) ! ok
    INTEGER, KIND :: KIND=0
    INTEGER, LEN  :: LEN=0
  END TYPE

  TYPE, EXTENDS(DT01) :: DT7(K, L)
  END TYPE

  TYPE :: DT8(K)
    INTEGER, KIND :: K
    INTEGER, KIND :: K
  END TYPE

  TYPE :: DT9(K)
    INTEGER, KIND :: K
    INTEGER       :: K
  END TYPE

  TYPE :: DT10(K)
    INTEGER, KIND, PRIVATE :: K
  END TYPE


  END


