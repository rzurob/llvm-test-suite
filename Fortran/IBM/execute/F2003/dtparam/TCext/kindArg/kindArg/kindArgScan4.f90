! GB DTP extension using:
! ftcx_dtp -qck -ql -qnodeferredlp /tstdev/F2003/kindArg/kindArg/kindArgScan4.f
! opt variations: -qnock -qnol -qdeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgScan4
!*
!*  DATE                       : Jun. 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SCAN
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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgScan4
  IMPLICIT NONE

  TYPE :: DT(N1,D1,D2,D3,D4)    ! (20,1,2,4,8)
    INTEGER, KIND            :: D1,D2,D3,D4
    INTEGER, LEN             :: N1
    INTEGER(D1), ALLOCATABLE :: K1
    INTEGER(D2), POINTER         :: K2
    INTEGER(D3), ALLOCATABLE :: K4
    INTEGER(D4), POINTER         :: K8
    CHARACTER(:), ALLOCATABLE :: CC(:)
  END TYPE

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4, I
  INTEGER(8) :: I8

  INTEGER                     :: II(128)=(/(I,I=0,127)/)

  TYPE(DT(20,1,2,4,8)), POINTER :: T

  ALLOCATE(T)

  ALLOCATE(T%K1, SOURCE=1_1)
  ALLOCATE(T%K2, SOURCE=2_2)
  ALLOCATE(T%K4, SOURCE=4_4)
  ALLOCATE(T%K8, SOURCE=8_8)

  ALLOCATE(T%CC(127), SOURCE=(/(CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(I)//CHAR(I), I=1, 127)/))

  DO I1 = 1, 127
    IF (     SCAN(STRING=T%CC(I1), SET=T%CC(I1)(7:7), KIND=KIND(T%K8))   .NE. 6)               STOP 11
    IF (KIND(SCAN(STRING=T%CC(I1), SET=T%CC(I1)(7:7), KIND=KIND(T%K8)))  .NE. 8)               STOP 12
    IF (     SCAN(STRING=T%CC(I1), SET=T%CC(I1)(7:7), KIND=KIND(T%K4))   .NE. 6)               STOP 13
    IF (KIND(SCAN(STRING=T%CC(I1), SET=T%CC(I1)(7:7), KIND=KIND(T%K4)))  .NE. 4)               STOP 14
    IF (     SCAN(STRING=T%CC(I1), SET=T%CC(I1)(7:7), KIND=KIND(T%K2))   .NE. 6)               STOP 15
    IF (KIND(SCAN(STRING=T%CC(I1), SET=T%CC(I1)(7:7), KIND=KIND(T%K2)))  .NE. 2)               STOP 16
    IF (     SCAN(STRING=T%CC(I1), SET=T%CC(I1)(7:7), KIND=KIND(T%K1))   .NE. 6)               STOP 17
    IF (KIND(SCAN(STRING=T%CC(I1), SET=T%CC(I1)(7:7), KIND=KIND(T%K1)))  .NE. 1)               STOP 18
  END DO

  DO I2 = 1, 127
    IF (     SCAN(STRING=T%CC(I2), SET=T%CC(I2)(7:7), BACK=.TRUE., KIND=KIND(T%K8))   .NE. 7)               STOP 21
    IF (KIND(SCAN(STRING=T%CC(I2), SET=T%CC(I2)(7:7), BACK=.TRUE., KIND=KIND(T%K8)))  .NE. 8)               STOP 22
    IF (     SCAN(STRING=T%CC(I2), SET=T%CC(I2)(7:7), BACK=.TRUE., KIND=KIND(T%K4))   .NE. 7)               STOP 23
    IF (KIND(SCAN(STRING=T%CC(I2), SET=T%CC(I2)(7:7), BACK=.TRUE., KIND=KIND(T%K4)))  .NE. 4)               STOP 24
    IF (     SCAN(STRING=T%CC(I2), SET=T%CC(I2)(7:7), BACK=.TRUE., KIND=KIND(T%K2))   .NE. 7)               STOP 25
    IF (KIND(SCAN(STRING=T%CC(I2), SET=T%CC(I2)(7:7), BACK=.TRUE., KIND=KIND(T%K2)))  .NE. 2)               STOP 26
    IF (     SCAN(STRING=T%CC(I2), SET=T%CC(I2)(7:7), BACK=.TRUE., KIND=KIND(T%K1))   .NE. 7)               STOP 27
    IF (KIND(SCAN(STRING=T%CC(I2), SET=T%CC(I2)(7:7), BACK=.TRUE., KIND=KIND(T%K1)))  .NE. 1)               STOP 28
  END DO

  DO I4 = 1, 127
    IF (     SCAN(STRING=T%CC(I4), SET=T%CC(I4)(6:7), BACK=.FALSE., KIND=KIND(T%K8))   .NE. 6)               STOP 41
    IF (KIND(SCAN(STRING=T%CC(I4), SET=T%CC(I4)(6:7), BACK=.FALSE., KIND=KIND(T%K8)))  .NE. 8)               STOP 42
    IF (     SCAN(STRING=T%CC(I4), SET=T%CC(I4)(6:7), BACK=.FALSE., KIND=KIND(T%K4))   .NE. 6)               STOP 43
    IF (KIND(SCAN(STRING=T%CC(I4), SET=T%CC(I4)(6:7), BACK=.FALSE., KIND=KIND(T%K4)))  .NE. 4)               STOP 44
    IF (     SCAN(STRING=T%CC(I4), SET=T%CC(I4)(6:7), BACK=.FALSE., KIND=KIND(T%K2))   .NE. 6)               STOP 45
    IF (KIND(SCAN(STRING=T%CC(I4), SET=T%CC(I4)(6:7), BACK=.FALSE., KIND=KIND(T%K2)))  .NE. 2)               STOP 46
    IF (     SCAN(STRING=T%CC(I4), SET=T%CC(I4)(6:7), BACK=.FALSE., KIND=KIND(T%K1))   .NE. 6)               STOP 47
    IF (KIND(SCAN(STRING=T%CC(I4), SET=T%CC(I4)(6:7), BACK=.FALSE., KIND=KIND(T%K1)))  .NE. 1)               STOP 48
  END DO

  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), KIND=KIND(T%K8))   .NE. 6))           STOP 51
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), KIND=KIND(T%K8)))  .NE. 8)            STOP 52
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), KIND=KIND(T%K4))   .NE. 6))           STOP 53
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), KIND=KIND(T%K4)))  .NE. 4)            STOP 54
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), KIND=KIND(T%K2))   .NE. 6))           STOP 55
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), KIND=KIND(T%K2)))  .NE. 2)            STOP 56
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), KIND=KIND(T%K1))   .NE. 6))           STOP 57
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), KIND=KIND(T%K1)))  .NE. 1)            STOP 58

  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.FALSE., KIND=KIND(T%K8))   .NE. 6))         STOP 61
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.FALSE., KIND=KIND(T%K8)))  .NE. 8)          STOP 62
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.FALSE., KIND=KIND(T%K4))   .NE. 6))         STOP 63
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.FALSE., KIND=KIND(T%K4)))  .NE. 4)          STOP 64
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.FALSE., KIND=KIND(T%K2))   .NE. 6))         STOP 65
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.FALSE., KIND=KIND(T%K2)))  .NE. 2)          STOP 66
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.FALSE., KIND=KIND(T%K1))   .NE. 6))         STOP 67
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.FALSE., KIND=KIND(T%K1)))  .NE. 1)          STOP 68

  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.TRUE., KIND=KIND(T%K8))   .NE. 7))           STOP 71
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.TRUE., KIND=KIND(T%K8)))  .NE. 8)            STOP 72
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.TRUE., KIND=KIND(T%K4))   .NE. 7))           STOP 73
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.TRUE., KIND=KIND(T%K4)))  .NE. 4)            STOP 74
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.TRUE., KIND=KIND(T%K2))   .NE. 7))           STOP 75
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.TRUE., KIND=KIND(T%K2)))  .NE. 2)            STOP 76
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.TRUE., KIND=KIND(T%K1))   .NE. 7))           STOP 77
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(7:7), BACK=.TRUE., KIND=KIND(T%K1)))  .NE. 1)            STOP 78



  END

