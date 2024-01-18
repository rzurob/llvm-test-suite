!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dtParamTypParamApp  
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
!*   type parameter may be used as a primary in a specification expression (7.1.6)
!*   in the derived-type-def. 
!*
!*  (failed at 11&ICE/336206)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  CONTAINS
  
  FUNCTION CF()
  CHARACTER(3) CF
    CF="123"
  END FUNCTION

  FUNCTION IntF()
  INTEGER(1) :: IntF
    IntF = 1
  END FUNCTION

  END MODULE

  PROGRAM dtParamTypParamApp 
  USE M
  IMPLICIT NONE

  TYPE :: DT0(K1, K2, K, K8)
    INTEGER(KIND=1), KIND :: K1
    INTEGER(KIND=2), KIND :: K2
    INTEGER,         KIND :: K 
    INTEGER(KIND=8), KIND :: K8

    CHARACTER(LEN=1) :: C1(K1) = "1"
    CHARACTER(LEN=2) :: C2(K2) = "2"
    CHARACTER(LEN=4) :: C (K)  = "4"
    CHARACTER(LEN=8) :: C8(K8) = "8"
  END TYPE

  TYPE :: DT1(L1, L2, L, L8)
    INTEGER(KIND=1), LEN :: L1
    INTEGER(KIND=2), LEN :: L2
    INTEGER,         LEN :: L 
    INTEGER(KIND=8), LEN :: L8

    CHARACTER(LEN=L1) :: C1(L1) = "1"
    CHARACTER(LEN=L2) :: C2(L2) = "2"
    CHARACTER(LEN=L)  :: C (L)  = "4"
    CHARACTER(LEN=L8) :: C8(L8) = "8"
  END TYPE

  TYPE :: DT2(K, L)
    INTEGER,         KIND :: K
    INTEGER,         LEN  :: L

    PROCEDURE(CHARACTER(L)), POINTER, NOPASS :: ProcPtr1=>NULL()
    PROCEDURE(INTEGER(K)),   POINTER, NOPASS :: ProcPtr2=>NULL()
  END TYPE

  TYPE(DT0(1, 2, 4, 8)) :: T1
  TYPE(DT1(1, 2, 4, 8)) :: T2
  TYPE(DT2(1,3))        :: T3

  IF ( LEN(T1%C1)       .NE. 1   )    STOP 11
  IF ( SIZE(T1%C1)      .NE. 1   )    STOP 12
  IF ( TRIM(T1%C1(1))   .NE. "1" )    STOP 13

  IF ( LEN(T1%C2)       .NE. 2   )    STOP 21
  IF ( SIZE(T1%C2)      .NE. 2   )    STOP 22
  IF ( TRIM(T1%C2(2))   .NE. "2" )    STOP 23

  IF ( LEN(T1%C )       .NE. 4   )    STOP 41
  IF ( SIZE(T1%C )      .NE. 4   )    STOP 42
  IF ( TRIM(T1%C(4))    .NE. "4" )    STOP 43

  IF ( LEN(T1%C8)       .NE. 8   )    STOP 81
  IF ( SIZE(T1%C8)      .NE. 8   )    STOP 82
  IF ( TRIM(T1%C8(8))   .NE. "8" )    STOP 83


  IF ( LEN(T2%C1)       .NE. 1   )    STOP 11
  IF ( SIZE(T2%C1)      .NE. 1   )    STOP 12
  IF ( TRIM(T2%C1(1))   .NE. "1" )    STOP 13

  IF ( LEN(T2%C2)       .NE. 2   )    STOP 21
  IF ( SIZE(T2%C2)      .NE. 2   )    STOP 22
  IF ( TRIM(T2%C2(2))   .NE. "2" )    STOP 23

  IF ( LEN(T2%C )       .NE. 4   )    STOP 41
  IF ( SIZE(T2%C )      .NE. 4   )    STOP 42
  IF ( TRIM(T2%C(4))    .NE. "4" )    STOP 43

  IF ( LEN(T2%C8)       .NE. 8   )    STOP 81
  IF ( SIZE(T2%C8)      .NE. 8   )    STOP 82
  IF ( TRIM(T2%C8(8))   .NE. "8" )    STOP 83

  T3%ProcPtr1 => CF
  T3%ProcPtr2 => IntF
  IF ( T3%ProcPtr1()   .NE. "123"   )    STOP 92
  IF ( T3%ProcPtr2()   .NE. 1 )          STOP 93

  END


