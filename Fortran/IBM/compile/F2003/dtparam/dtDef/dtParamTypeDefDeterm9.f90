!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefDeterm9
!*
!*  DATE                       : Dec. 15, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Determination of Types
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
!*  Determination of derived types - sequence types
!*
!*  (335771)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  USE ISO_C_BINDING

  TYPE :: DT0(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
  END TYPE

  TYPE :: DT1(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
  END TYPE

  TYPE :: DT2(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    PRIVATE
    SEQUENCE
    INTEGER(K) :: I = K
  END TYPE

  TYPE, PRIVATE :: DT3(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
    INTEGER(K) :: I = K
    INTEGER(K) :: J = K
  END TYPE

  TYPE :: DT4(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
    INTEGER(K) :: I = K
    INTEGER(K) :: J = K
  END TYPE

  TYPE, PRIVATE :: DT5(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
  END TYPE

  TYPE :: DT6(K)
    INTEGER, KIND :: K
    SEQUENCE
  END TYPE

  TYPE :: DT7(K)
    INTEGER(4), KIND :: K
    SEQUENCE
  END TYPE

  TYPE, BIND(C) :: DT9
    INTEGER(C_INT) :: K
  END TYPE

  TYPE(DT0(4, 4)) :: T0
  TYPE(DT1(4, 4)) :: T1
  TYPE(DT2(4, 4)), SAVE :: T2
  TYPE(DT3(4, 4)), SAVE :: T3
  TYPE(DT4(4, 4)), SAVE :: T4
  TYPE(DT5(4, 4)) :: T5
  TYPE(DT6(4   )) :: T6
  TYPE(DT7(4   )) :: T7
  TYPE(DT9)       :: T9


  END MODULE

  PROGRAM dtParamTypeDefDeterm9
  USE M, ONLY: T0, T1, T2, T3, T4, T5, T6, T7, T9
  USE ISO_C_BINDING

  TYPE :: DT0(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
  END TYPE

  TYPE :: DT1(K, L)
    INTEGER, LEN  :: L
    INTEGER, KIND :: K
    SEQUENCE
  END TYPE

  TYPE :: DT2(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
  ! INTEGER(K) :: I = K
  END TYPE

  TYPE :: DT3(K, L)
    INTEGER, KIND :: K=0
    INTEGER, LEN  :: L=0
    SEQUENCE
    INTEGER(K) :: I = K+K
    INTEGER(K) :: J = 5*K
  END TYPE

  TYPE :: DT4(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
    INTEGER(K) :: J
    INTEGER(K) :: I
  END TYPE

  TYPE :: DT5(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
  END TYPE

  TYPE :: DT6(KK)
    INTEGER, KIND :: KK
    SEQUENCE
  END TYPE

  TYPE :: DT7(K)
    INTEGER(1), KIND :: K
    SEQUENCE
  END TYPE

  TYPE, BIND(C) :: DT9
    INTEGER(C_INT) :: K
  END TYPE

  TYPE(DT0(4, 4)) :: T00

  TYPE(DT1(4, 4)) :: T11

  TYPE(DT2(4, 4)) :: T22

  TYPE(DT3(4, 4)) :: T33

  TYPE(DT4(4, 4)) :: T44

  TYPE(DT5(4, 4)) :: T55

  TYPE(DT6(4   )) :: T66

  TYPE(DT7(4   )) :: T77

  TYPE(DT9)       :: T99


  t0 = T00 ! This should be fine

  T1 = T11 ! This should be fine

  T2 = T22

  T3 = T33 ! This should be fine

  T4 = T44

  T5 = T55 ! This should be fine

  T6 = T66

  T7 = T77

  T9 = T99 ! This should be fine

  END

