!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : IACHAR
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
!*  Integer. If KIND is present, the kind type parameter is that specified
!*  by the value of KIND; otherwise, the kind type parameter is that of default integer type.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIachar5


  INTEGER(1), ALLOCATABLE :: I1, K1
  INTEGER(2), POINTER     :: I2, K2
  INTEGER(4), ALLOCATABLE :: I4, K4
  INTEGER(8), POINTER     :: I8, K8

  INTEGER                 :: II(128)=(/(I,I=0,127)/)
  CHARACTER               :: CC(128)=(/(ACHAR(I),I=0,127)/)

  ALLOCATE(I1)
  ALLOCATE(I2)
  ALLOCATE(I4)
  ALLOCATE(I8)

  ALLOCATE(K1, SOURCE=1_1)
  ALLOCATE(K2, SOURCE=1_2)
  ALLOCATE(K4, SOURCE=1_4)
  ALLOCATE(K8, SOURCE=1_8)


  DO I1 = 0, 127
    IF (     IACHAR(C=ACHAR(I1, KIND=K1%KIND   ), KIND=KIND(K8))   .NE. I1)               STOP 11
    IF (KIND(IACHAR(C=ACHAR(I1, KIND=K1%KIND   ), KIND=KIND(K8)))  .NE. 8)                STOP 12
    IF (     IACHAR(C=ACHAR(I1, KIND=K2%KIND-1 ), KIND=KIND(K4))   .NE. I1)               STOP 13
    IF (KIND(IACHAR(C=ACHAR(I1, KIND=K2%KIND-1 ), KIND=KIND(K4)))  .NE. 4)                STOP 14
    IF (     IACHAR(C=ACHAR(I1, KIND=K4%KIND-3 ), KIND=KIND(K2))   .NE. I1)               STOP 15
    IF (KIND(IACHAR(C=ACHAR(I1, KIND=K4%KIND-3 ), KIND=KIND(K2)))  .NE. 2)                STOP 16
    IF (     IACHAR(C=ACHAR(I1, KIND=K8%KIND-7 ), KIND=KIND(K1))   .NE. I1)               STOP 17
    IF (KIND(IACHAR(C=ACHAR(I1, KIND=K8%KIND-7 ), KIND=KIND(K1)))  .NE. 1)                STOP 18
  END DO

  DO I2 = 0, 127
    IF (     IACHAR(C=ACHAR(I2, KIND=K1%KIND   ), KIND=KIND(K8))   .NE. I2)               STOP 21
    IF (KIND(IACHAR(C=ACHAR(I2, KIND=K1%KIND   ), KIND=KIND(K8)))  .NE. 8)                STOP 22
    IF (     IACHAR(C=ACHAR(I2, KIND=K2%KIND-1 ), KIND=KIND(K4))   .NE. I2)               STOP 23
    IF (KIND(IACHAR(C=ACHAR(I2, KIND=K2%KIND-1 ), KIND=KIND(K4)))  .NE. 4)                STOP 24
    IF (     IACHAR(C=ACHAR(I2, KIND=K4%KIND-3 ), KIND=KIND(K2))   .NE. I2)               STOP 25
    IF (KIND(IACHAR(C=ACHAR(I2, KIND=K4%KIND-3 ), KIND=KIND(K2)))  .NE. 2)                STOP 26
    IF (     IACHAR(C=ACHAR(I2, KIND=K8%KIND-7 ), KIND=KIND(K1))   .NE. I2)               STOP 27
    IF (KIND(IACHAR(C=ACHAR(I2, KIND=K8%KIND-7 ), KIND=KIND(K1)))  .NE. 1)                STOP 28
  END DO

  DO I4 = 0, 127
    IF (     IACHAR(C=ACHAR(I4, KIND=K1%KIND   ), KIND=KIND(K8))   .NE. I4)               STOP 41
    IF (KIND(IACHAR(C=ACHAR(I4, KIND=K1%KIND   ), KIND=KIND(K8)))  .NE. 8)                STOP 42
    IF (     IACHAR(C=ACHAR(I4, KIND=K2%KIND-1 ), KIND=KIND(K4))   .NE. I4)               STOP 43
    IF (KIND(IACHAR(C=ACHAR(I4, KIND=K2%KIND-1 ), KIND=KIND(K4)))  .NE. 4)                STOP 44
    IF (     IACHAR(C=ACHAR(I4, KIND=K4%KIND-3 ), KIND=KIND(K2))   .NE. I4)               STOP 45
    IF (KIND(IACHAR(C=ACHAR(I4, KIND=K4%KIND-3 ), KIND=KIND(K2)))  .NE. 2)                STOP 46
    IF (     IACHAR(C=ACHAR(I4, KIND=K8%KIND-7 ), KIND=KIND(K1))   .NE. I4)               STOP 47
    IF (KIND(IACHAR(C=ACHAR(I4, KIND=K8%KIND-7 ), KIND=KIND(K1)))  .NE. 1)                STOP 48
  END DO

  DO I8 = 0, 127
    IF (     IACHAR(C=ACHAR(I8, KIND=K1%KIND   ), KIND=KIND(K8))   .NE. I8)               STOP 81
    IF (KIND(IACHAR(C=ACHAR(I8, KIND=K1%KIND   ), KIND=KIND(K8)))  .NE. 8)                STOP 82
    IF (     IACHAR(C=ACHAR(I8, KIND=K2%KIND-1 ), KIND=KIND(K4))   .NE. I8)               STOP 83
    IF (KIND(IACHAR(C=ACHAR(I8, KIND=K2%KIND-1 ), KIND=KIND(K4)))  .NE. 4)                STOP 84
    IF (     IACHAR(C=ACHAR(I8, KIND=K4%KIND-3 ), KIND=KIND(K2))   .NE. I8)               STOP 85
    IF (KIND(IACHAR(C=ACHAR(I8, KIND=K4%KIND-3 ), KIND=KIND(K2)))  .NE. 2)                STOP 86
    IF (     IACHAR(C=ACHAR(I8, KIND=K8%KIND-7 ), KIND=KIND(K1))   .NE. I8)               STOP 87
    IF (KIND(IACHAR(C=ACHAR(I8, KIND=K8%KIND-7 ), KIND=KIND(K1)))  .NE. 1)                STOP 88
  END DO



  IF (ANY( IACHAR(C=ACHAR(I=II, KIND=K1%KIND   ), KIND=KIND(K8))   .NE. II))           STOP 11
  IF (KIND(IACHAR(C=ACHAR(I=II, KIND=K1%KIND   ), KIND=KIND(K8)))  .NE. 8)             STOP 12
  IF (ANY( IACHAR(C=ACHAR(I=II, KIND=K2%KIND-1 ), KIND=KIND(K4))   .NE. II))           STOP 13
  IF (KIND(IACHAR(C=ACHAR(I=II, KIND=K2%KIND-1 ), KIND=KIND(K4)))  .NE. 4)             STOP 14
  IF (ANY( IACHAR(C=ACHAR(I=II, KIND=K4%KIND-3 ), KIND=KIND(K2))   .NE. II))           STOP 15
  IF (KIND(IACHAR(C=ACHAR(I=II, KIND=K4%KIND-3 ), KIND=KIND(K2)))  .NE. 2)             STOP 16
  IF (ANY( IACHAR(C=ACHAR(I=II, KIND=K8%KIND-7 ), KIND=KIND(K1))   .NE. II))           STOP 17
  IF (KIND(IACHAR(C=ACHAR(I=II, KIND=K8%KIND-7 ), KIND=KIND(K1)))  .NE. 1)             STOP 18




  END

