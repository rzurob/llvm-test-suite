!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgVerify5
!*
!*  DATE                       : Jul. 06, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : VERIFY
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Result Characteristics.
!*  Integer. If KIND is present, the kind type parameter is that specified by the value of KIND;
!*  otherwise the kind type parameter is that of default integer type.
!*
!*  (322447)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgVerify5
  IMPLICIT NONE

  TYPE :: DT
    INTEGER(1) :: K1=0
    INTEGER(2) :: K2=0
    INTEGER(4) :: K4=0
    INTEGER(8) :: K8=0
    CHARACTER  :: CC=""
  END TYPE

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4, I, J
  INTEGER(8) :: I8

  INTEGER                   :: II(128)=(/(I,I=0,127)/)

  TYPE(DT),       PARAMETER :: T(128)=DT()
  CHARACTER(128), POINTER   :: CC(:)


  ALLOCATE(CC(0:127))
  DO I=0, 127
    DO J=1, 128
      CC(I)(J:J)=ACHAR(0)
    END DO
    CC(I)(1:1)=ACHAR(I)
  END DO

  DO I1 = 1, 127
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(1:1), KIND=KIND(T%K8))   .NE. 2)               STOP 11
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(1:1), KIND=KIND(T%K8)))  .NE. 8)               STOP 12
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(1:1), KIND=KIND(T%K4))   .NE. 2)               STOP 13
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(1:1), KIND=KIND(T%K4)))  .NE. 4)               STOP 14
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(1:1), KIND=KIND(T%K2))   .NE. 2)               STOP 15
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(1:1), KIND=KIND(T%K2)))  .NE. 2)               STOP 16
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(1:1), KIND=KIND(T%K1))   .NE. 2)               STOP 17
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(1:1), KIND=KIND(T%K1)))  .NE. 1)               STOP 18
  END DO

  DO I2 = 1, 127
    IF (     VERIFY(STRING=CC(I2), SET=CC(I2)(1:1), KIND=KIND(T%K8))   .NE. 2)               STOP 21
    IF (KIND(VERIFY(STRING=CC(I2), SET=CC(I2)(1:1), KIND=KIND(T%K8)))  .NE. 8)               STOP 22
    IF (     VERIFY(STRING=CC(I2), SET=CC(I2)(1:1), KIND=KIND(T%K4))   .NE. 2)               STOP 23
    IF (KIND(VERIFY(STRING=CC(I2), SET=CC(I2)(1:1), KIND=KIND(T%K4)))  .NE. 4)               STOP 24
    IF (     VERIFY(STRING=CC(I2), SET=CC(I2)(1:1), KIND=KIND(T%K2))   .NE. 2)               STOP 25
    IF (KIND(VERIFY(STRING=CC(I2), SET=CC(I2)(1:1), KIND=KIND(T%K2)))  .NE. 2)               STOP 26
    IF (     VERIFY(STRING=CC(I2), SET=CC(I2)(1:1), KIND=KIND(T%K1))   .NE. 2)               STOP 27
    IF (KIND(VERIFY(STRING=CC(I2), SET=CC(I2)(1:1), KIND=KIND(T%K1)))  .NE. 1)               STOP 28
  END DO

  DO I4 = 1, 127
    IF (     VERIFY(STRING=CC(I4), SET=CC(I4)(1:1), KIND=KIND(T%K8))   .NE. 2)               STOP 41
    IF (KIND(VERIFY(STRING=CC(I4), SET=CC(I4)(1:1), KIND=KIND(T%K8)))  .NE. 8)               STOP 42
    IF (     VERIFY(STRING=CC(I4), SET=CC(I4)(1:1), KIND=KIND(T%K4))   .NE. 2)               STOP 43
    IF (KIND(VERIFY(STRING=CC(I4), SET=CC(I4)(1:1), KIND=KIND(T%K4)))  .NE. 4)               STOP 44
    IF (     VERIFY(STRING=CC(I4), SET=CC(I4)(1:1), KIND=KIND(T%K2))   .NE. 2)               STOP 45
    IF (KIND(VERIFY(STRING=CC(I4), SET=CC(I4)(1:1), KIND=KIND(T%K2)))  .NE. 2)               STOP 46
    IF (     VERIFY(STRING=CC(I4), SET=CC(I4)(1:1), KIND=KIND(T%K1))   .NE. 2)               STOP 47
    IF (KIND(VERIFY(STRING=CC(I4), SET=CC(I4)(1:1), KIND=KIND(T%K1)))  .NE. 1)               STOP 48
  END DO

  DO I8 = 1, 127
    IF (     VERIFY(STRING=CC(I8), SET=CC(I8)(1:1), KIND=KIND(T%K8))   .NE. 2)               STOP 83
    IF (KIND(VERIFY(STRING=CC(I8), SET=CC(I8)(1:1), KIND=KIND(T%K8)))  .NE. 8)               STOP 82
    IF (     VERIFY(STRING=CC(I8), SET=CC(I8)(1:1), KIND=KIND(T%K4))   .NE. 2)               STOP 83
    IF (KIND(VERIFY(STRING=CC(I8), SET=CC(I8)(1:1), KIND=KIND(T%K4)))  .NE. 4)               STOP 84
    IF (     VERIFY(STRING=CC(I8), SET=CC(I8)(1:1), KIND=KIND(T%K2))   .NE. 2)               STOP 85
    IF (KIND(VERIFY(STRING=CC(I8), SET=CC(I8)(1:1), KIND=KIND(T%K2)))  .NE. 2)               STOP 86
    IF (     VERIFY(STRING=CC(I8), SET=CC(I8)(1:1), KIND=KIND(T%K1))   .NE. 2)               STOP 87
    IF (KIND(VERIFY(STRING=CC(I8), SET=CC(I8)(1:1), KIND=KIND(T%K1)))  .NE. 1)               STOP 88
  END DO


  IF (ANY( VERIFY(STRING=CC(1:), SET=CC(1:)(1:1), KIND=KIND(T%K8))   .NE. 2))           STOP 91
  IF (KIND(VERIFY(STRING=CC(1:), SET=CC(1:)(1:1), KIND=KIND(T%K8)))  .NE. 8)            STOP 92
  IF (ANY( VERIFY(STRING=CC(1:), SET=CC(1:)(1:1), KIND=KIND(T%K4))   .NE. 2))           STOP 93
  IF (KIND(VERIFY(STRING=CC(1:), SET=CC(1:)(1:1), KIND=KIND(T%K4)))  .NE. 4)            STOP 94
  IF (ANY( VERIFY(STRING=CC(1:), SET=CC(1:)(1:1), KIND=KIND(T%K2))   .NE. 2))           STOP 95
  IF (KIND(VERIFY(STRING=CC(1:), SET=CC(1:)(1:1), KIND=KIND(T%K2)))  .NE. 2)            STOP 96
  IF (ANY( VERIFY(STRING=CC(1:), SET=CC(1:)(1:1), KIND=KIND(T%K1))   .NE. 2))           STOP 97
  IF (KIND(VERIFY(STRING=CC(1:), SET=CC(1:)(1:1), KIND=KIND(T%K1)))  .NE. 1)            STOP 98




  END

