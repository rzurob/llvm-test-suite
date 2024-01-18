!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 30, 2006
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
!*  A kind type parameter may also be used as a primary in an initialization expression (7.1.7)
!*  in the derived-type-def.
!*
!*  (ice)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypParamApp1
  IMPLICIT NONE

  TYPE :: DT0(K1, K2, K, K8)
    INTEGER(KIND=1), KIND :: K1
    INTEGER(KIND=2), KIND :: K2
    INTEGER,         KIND :: K
    INTEGER(KIND=8), KIND :: K8

    CHARACTER(LEN=1) :: C1(K1) = CHAR(1)
    CHARACTER(LEN=2) :: C2(K2) = CHAR(2)
    CHARACTER(LEN=4)  :: C (K)  = CHAR(4)
    CHARACTER(LEN=8) :: C8(K8) = CHAR(8)
  END TYPE

  TYPE, EXTENDS(DT0) :: DT1
    INTEGER(KIND=1) :: I1(1)=k1
    INTEGER(KIND=2) :: I2(2)=k2
    INTEGER(KIND=4) :: I (4)=k
    INTEGER(KIND=8) :: I8(8)=k8
  END TYPE


  TYPE(DT0(1, 2, 4, 8)) :: T1
  TYPE(DT1(1, 2, 4, 8)) :: T2

  IF ( LEN(T1%C1)       .NE. 1   )    STOP 11
  IF ( SIZE(T1%C1)      .NE. 1   )    STOP 12
  IF ( T1%C1(1)         .NE. CHAR(1)) STOP 13

  IF ( LEN(T1%C2)       .NE. 2   )    STOP 21
  IF ( SIZE(T1%C2)      .NE. 2   )    STOP 22
  IF ( T1%C2(2)         .NE. CHAR(2)) STOP 23

  IF ( LEN(T1%C )       .NE. 4   )    STOP 41
  IF ( SIZE(T1%C )      .NE. 4   )    STOP 42
  IF ( T1%C(4)          .NE. CHAR(4)) STOP 43

  IF ( LEN(T1%C8)       .NE. 8   )    STOP 81
  IF ( SIZE(T1%C8)      .NE. 8   )    STOP 82
  IF ( T1%C8(8)         .NE. CHAR(8)) STOP 83

  IF ( KIND(T2%I1)      .NE. 1   )    STOP 11
  IF ( SIZE(T2%I1)      .NE. 1   )    STOP 12
  IF ( ANY(T2%I1        .NE. 1))  STOP 13

  IF ( KIND(T2%I2)      .NE. 2   )    STOP 21
  IF ( SIZE(T2%I2)      .NE. 2   )    STOP 22
  IF ( ANY(T2%I2        .NE. (/2,2/)))             STOP 23

  IF ( KIND(T2%I )      .NE. 4   )    STOP 41
  IF ( SIZE(T2%I )      .NE. 4   )    STOP 42
  IF ( ANY(T2%I         .NE. (/4,4,4,4/)))         STOP 43

  IF ( KIND(T2%I8)      .NE. 8   )    STOP 81
  IF ( SIZE(T2%I8)      .NE. 8   )    STOP 82
  IF ( ANY(T2%I8        .NE. (/8,8,8,8,8,8,8,8/))) STOP 83



  END


