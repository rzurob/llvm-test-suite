!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 27, 2006
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
!*  Each type parameter is itself of type integer. If its kind selector is omitted,
!*  the kind type parameter is default integer
!*
!*  (not recog KIND/340263)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  dtParamTypParamKind

  TYPE :: DT0(K1, K2, K, K4, K8)
    INTEGER(KIND=1), KIND :: K1=1_1
    INTEGER(KIND=2), KIND :: K2=2_2
    INTEGER,         KIND :: K =4
    INTEGER(KIND=4), KIND :: K4=4_4
    INTEGER(KIND=8), KIND :: K8=8_8

    INTEGER(KIND=KIND(K1)) :: IK1=K1
    INTEGER(KIND=KIND(K2)) :: IK2=K2
    INTEGER(KIND=KIND(K))  :: IK=K
    INTEGER(KIND=KIND(K4)) :: IK4=K4
    INTEGER(KIND=KIND(K8)) :: IK8=K8
  END TYPE


  TYPE, EXTENDS(DT0) :: DT1(L1, L2, L, L4, L8)
    INTEGER(KIND=1), LEN  :: L1=1_1
    INTEGER(KIND=2), LEN  :: L2=2_2
    INTEGER,         LEN  :: L =4
    INTEGER(KIND=4), LEN  :: L4=4_4
    INTEGER(KIND=8), LEN  :: L8=8_8

    CHARACTER(LEN=KIND(L1)) :: CL1="1"
    CHARACTER(LEN=KIND(L2)) :: CL2="2"
    CHARACTER(LEN=KIND(L))  :: CL="4"
    CHARACTER(LEN=KIND(L4)) :: CL4="4"
    CHARACTER(LEN=KIND(L8)) :: CL8="8"
  END TYPE

  TYPE(DT0) :: T1
  TYPE(DT1) :: T2

  IF ( KIND(T1%IK1) .NE. 1 )      ERROR STOP 11
  IF ( T1%IK1       .NE. 1 )      ERROR STOP 12

  IF ( KIND(T1%IK2) .NE. 2 )      ERROR STOP 21
  IF ( T1%IK2       .NE. 2 )      ERROR STOP 22

  IF ( KIND(T1%IK)  .NE. 4 )      ERROR STOP 31
  IF ( T1%IK        .NE. 4 )      ERROR STOP 32

  IF ( KIND(T1%IK4) .NE. 4 )      ERROR STOP 41
  IF ( T1%IK4       .NE. 4 )      ERROR STOP 42

  IF ( KIND(T1%IK8) .NE. 8 )      ERROR STOP 51
  IF ( T1%IK8       .NE. 8 )      ERROR STOP 52


  IF ( LEN(T2%CL1)   .NE. 1 )      ERROR STOP 61
  IF ( TRIM(T2%CL1)  .NE. "1" )    ERROR STOP 62

  IF ( LEN(T2%CL2)   .NE. 2 )      ERROR STOP 71
  IF ( TRIM(T2%CL2)  .NE. "2" )    ERROR STOP 72

  IF ( LEN(T2%CL)    .NE. 4 )      ERROR STOP 81
  IF ( TRIM(T2%CL)   .NE. "4" )    ERROR STOP 82

  IF ( LEN(T2%CL4)   .NE. 4 )      ERROR STOP 91
  IF ( TRIM(T2%CL4)  .NE. "4" )    ERROR STOP 92

  IF ( LEN(T2%CL8)   .NE. 8 )      ERROR STOP 101
  IF ( TRIM(T2%CL8)  .NE. "8" )    ERROR STOP 102

  END


