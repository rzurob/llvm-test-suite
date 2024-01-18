! GB DTP extension using:
! ftcx_dtp -qck -ql -qnodeferredlp /tstdev/F2003/kindArg/kindArg/kindArgIndex5.f
! opt variations: -qnock -qnol -qdeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIndex5
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : INDEX
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
!*  otherwise, the kind type parameter is that of default integer type.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIndex5
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
  INTEGER(4) :: I4, I, J
  INTEGER(8) :: I8

  INTEGER                     :: II(127)=(/(I,I=1,127)/)

  TYPE(DT(20,1,2,4,8)), POINTER :: T

  ALLOCATE(T)

  ALLOCATE(T%K1, SOURCE=1_1)
  ALLOCATE(T%K2, SOURCE=2_2)
  ALLOCATE(T%K4, SOURCE=4_4)
  ALLOCATE(T%K8, SOURCE=8_8)

  ALLOCATE(CHARACTER(128) :: T%CC(1:127))

  DO I = 1, 127
  DO J = 1, 127
    IF (I .EQ. J) THEN
      T%CC(I)(J:J)= ACHAR(0)
    ELSE
      T%CC(I)(J:J)= ACHAR(I)
    END IF
  END DO
  END DO

  DO I1 = 1, 127
    IF (     INDEX(STRING=T%CC(I1), SUBSTRING=CHAR(0), KIND=KIND(T%K8))   .NE. I1)               STOP 11
    IF (KIND(INDEX(STRING=T%CC(I1), SUBSTRING=CHAR(0), KIND=KIND(T%K8)))  .NE. 8)                STOP 12
    IF (     INDEX(STRING=T%CC(I1), SUBSTRING=CHAR(0), KIND=KIND(T%K4))   .NE. I1)               STOP 13
    IF (KIND(INDEX(STRING=T%CC(I1), SUBSTRING=CHAR(0), KIND=KIND(T%K4)))  .NE. 4)                STOP 14
    IF (     INDEX(STRING=T%CC(I1), SUBSTRING=CHAR(0), KIND=KIND(T%K2))   .NE. I1)               STOP 15
    IF (KIND(INDEX(STRING=T%CC(I1), SUBSTRING=CHAR(0), KIND=KIND(T%K2)))  .NE. 2)                STOP 16
    IF (     INDEX(STRING=T%CC(I1), SUBSTRING=CHAR(0), KIND=KIND(T%K1))   .NE. I1)               STOP 17
    IF (KIND(INDEX(STRING=T%CC(I1), SUBSTRING=CHAR(0), KIND=KIND(T%K1)))  .NE. 1)                STOP 18
  END DO

  DO I2 = 1, 127
    IF (     INDEX(STRING=T%CC(I2), SUBSTRING=CHAR(0), KIND=KIND(T%K8))   .NE. I2)               STOP 21
    IF (KIND(INDEX(STRING=T%CC(I2), SUBSTRING=CHAR(0), KIND=KIND(T%K8)))  .NE. 8)                STOP 22
    IF (     INDEX(STRING=T%CC(I2), SUBSTRING=CHAR(0), KIND=KIND(T%K4))   .NE. I2)               STOP 23
    IF (KIND(INDEX(STRING=T%CC(I2), SUBSTRING=CHAR(0), KIND=KIND(T%K4)))  .NE. 4)                STOP 24
    IF (     INDEX(STRING=T%CC(I2), SUBSTRING=CHAR(0), KIND=KIND(T%K2))   .NE. I2)               STOP 25
    IF (KIND(INDEX(STRING=T%CC(I2), SUBSTRING=CHAR(0), KIND=KIND(T%K2)))  .NE. 2)                STOP 26
    IF (     INDEX(STRING=T%CC(I2), SUBSTRING=CHAR(0), KIND=KIND(T%K1))   .NE. I2)               STOP 27
    IF (KIND(INDEX(STRING=T%CC(I2), SUBSTRING=CHAR(0), KIND=KIND(T%K1)))  .NE. 1)                STOP 28
  END DO

  DO I4 = 1, 127
    IF (     INDEX(STRING=T%CC(I4), SUBSTRING=CHAR(0), KIND=KIND(T%K8))   .NE. I4)               STOP 41
    IF (KIND(INDEX(STRING=T%CC(I4), SUBSTRING=CHAR(0), KIND=KIND(T%K8)))  .NE. 8)                STOP 42
    IF (     INDEX(STRING=T%CC(I4), SUBSTRING=CHAR(0), KIND=KIND(T%K4))   .NE. I4)               STOP 43
    IF (KIND(INDEX(STRING=T%CC(I4), SUBSTRING=CHAR(0), KIND=KIND(T%K4)))  .NE. 4)                STOP 44
    IF (     INDEX(STRING=T%CC(I4), SUBSTRING=CHAR(0), KIND=KIND(T%K2))   .NE. I4)               STOP 45
    IF (KIND(INDEX(STRING=T%CC(I4), SUBSTRING=CHAR(0), KIND=KIND(T%K2)))  .NE. 2)                STOP 46
    IF (     INDEX(STRING=T%CC(I4), SUBSTRING=CHAR(0), KIND=KIND(T%K1))   .NE. I4)               STOP 47
    IF (KIND(INDEX(STRING=T%CC(I4), SUBSTRING=CHAR(0), KIND=KIND(T%K1)))  .NE. 1)                STOP 48
  END DO

  DO I8 = 1, 127
    IF (     INDEX(STRING=T%CC(I8), SUBSTRING=CHAR(0), KIND=KIND(T%K8))   .NE. I8)               STOP 83
    IF (KIND(INDEX(STRING=T%CC(I8), SUBSTRING=CHAR(0), KIND=KIND(T%K8)))  .NE. 8)                STOP 82
    IF (     INDEX(STRING=T%CC(I8), SUBSTRING=CHAR(0), KIND=KIND(T%K4))   .NE. I8)               STOP 83
    IF (KIND(INDEX(STRING=T%CC(I8), SUBSTRING=CHAR(0), KIND=KIND(T%K4)))  .NE. 4)                STOP 84
    IF (     INDEX(STRING=T%CC(I8), SUBSTRING=CHAR(0), KIND=KIND(T%K2))   .NE. I8)               STOP 85
    IF (KIND(INDEX(STRING=T%CC(I8), SUBSTRING=CHAR(0), KIND=KIND(T%K2)))  .NE. 2)                STOP 86
    IF (     INDEX(STRING=T%CC(I8), SUBSTRING=CHAR(0), KIND=KIND(T%K1))   .NE. I8)               STOP 87
    IF (KIND(INDEX(STRING=T%CC(I8), SUBSTRING=CHAR(0), KIND=KIND(T%K1)))  .NE. 1)                STOP 88
  END DO



  IF (ANY( INDEX(STRING=T%CC, SUBSTRING=CHAR(0), KIND=KIND(T%K8))   .NE. II))           STOP 91
  IF (KIND(INDEX(STRING=T%CC, SUBSTRING=CHAR(0), KIND=KIND(T%K8)))  .NE. 8)             STOP 92
  IF (ANY( INDEX(STRING=T%CC, SUBSTRING=CHAR(0), KIND=KIND(T%K4))   .NE. II))           STOP 93
  IF (KIND(INDEX(STRING=T%CC, SUBSTRING=CHAR(0), KIND=KIND(T%K4)))  .NE. 4)             STOP 94
  IF (ANY( INDEX(STRING=T%CC, SUBSTRING=CHAR(0), KIND=KIND(T%K2))   .NE. II))           STOP 95
  IF (KIND(INDEX(STRING=T%CC, SUBSTRING=CHAR(0), KIND=KIND(T%K2)))  .NE. 2)             STOP 96
  IF (ANY( INDEX(STRING=T%CC, SUBSTRING=CHAR(0), KIND=KIND(T%K1))   .NE. II))           STOP 97
  IF (KIND(INDEX(STRING=T%CC, SUBSTRING=CHAR(0), KIND=KIND(T%K1)))  .NE. 1)             STOP 98




  END

