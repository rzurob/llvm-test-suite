!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ICHAR
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


  PROGRAM kindArgIchar5


  INTEGER(1), ALLOCATABLE :: I1, K1
  INTEGER(2), POINTER     :: I2, K2
  INTEGER(4), ALLOCATABLE :: I4, K4
  INTEGER(8), POINTER     :: I8, K8

  INTEGER                     :: II(0:127)=(/(I,I=0,127)/)
  CHARACTER(:), ALLOCATABLE   :: CC(:)

  ALLOCATE(I1)
  ALLOCATE(I2)
  ALLOCATE(I4)
  ALLOCATE(I8)

  ALLOCATE(K1, SOURCE=1_1)
  ALLOCATE(K2, SOURCE=1_2)
  ALLOCATE(K4, SOURCE=1_4)
  ALLOCATE(K8, SOURCE=1_8)

  ALLOCATE(CC(0:127), SOURCE=(/(CHAR(I=I, KIND=1), I=0, 127)/))

  DO I1 = 0, 127
    IF (     ICHAR(C=CC(I1), KIND=KIND(K8))   .NE. I1)               ERROR STOP 11
    IF (KIND(ICHAR(C=CC(I1), KIND=KIND(K8)))  .NE. 8)                ERROR STOP 12
    IF (     ICHAR(C=CC(I1), KIND=KIND(K4))   .NE. I1)               ERROR STOP 13
    IF (KIND(ICHAR(C=CC(I1), KIND=KIND(K4)))  .NE. 4)                ERROR STOP 14
    IF (     ICHAR(C=CC(I1), KIND=KIND(K2))   .NE. I1)               ERROR STOP 15
    IF (KIND(ICHAR(C=CC(I1), KIND=KIND(K2)))  .NE. 2)                ERROR STOP 16
    IF (     ICHAR(C=CC(I1), KIND=KIND(K1))   .NE. I1)               ERROR STOP 17
    IF (KIND(ICHAR(C=CC(I1), KIND=KIND(K1)))  .NE. 1)                ERROR STOP 18
  END DO

  DO I2 = 0, 127
    IF (     ICHAR(C=CC(I2), KIND=KIND(K8))   .NE. I2)               ERROR STOP 21
    IF (KIND(ICHAR(C=CC(I2), KIND=KIND(K8)))  .NE. 8)                ERROR STOP 22
    IF (     ICHAR(C=CC(I2), KIND=KIND(K4))   .NE. I2)               ERROR STOP 23
    IF (KIND(ICHAR(C=CC(I2), KIND=KIND(K4)))  .NE. 4)                ERROR STOP 24
    IF (     ICHAR(C=CC(I2), KIND=KIND(K2))   .NE. I2)               ERROR STOP 25
    IF (KIND(ICHAR(C=CC(I2), KIND=KIND(K2)))  .NE. 2)                ERROR STOP 26
    IF (     ICHAR(C=CC(I2), KIND=KIND(K1))   .NE. I2)               ERROR STOP 27
    IF (KIND(ICHAR(C=CC(I2), KIND=KIND(K1)))  .NE. 1)                ERROR STOP 28
  END DO

  DO I4 = 0, 127
    IF (     ICHAR(C=CC(I4), KIND=KIND(K8))   .NE. I4)               ERROR STOP 41
    IF (KIND(ICHAR(C=CC(I4), KIND=KIND(K8)))  .NE. 8)                ERROR STOP 42
    IF (     ICHAR(C=CC(I4), KIND=KIND(K4))   .NE. I4)               ERROR STOP 43
    IF (KIND(ICHAR(C=CC(I4), KIND=KIND(K4)))  .NE. 4)                ERROR STOP 44
    IF (     ICHAR(C=CC(I4), KIND=KIND(K2))   .NE. I4)               ERROR STOP 45
    IF (KIND(ICHAR(C=CC(I4), KIND=KIND(K2)))  .NE. 2)                ERROR STOP 46
    IF (     ICHAR(C=CC(I4), KIND=KIND(K1))   .NE. I4)               ERROR STOP 47
    IF (KIND(ICHAR(C=CC(I4), KIND=KIND(K1)))  .NE. 1)                ERROR STOP 48
  END DO

  DO I8 = 0, 127
    IF (     ICHAR(C=CC(I8), KIND=KIND(K8))   .NE. I8)               ERROR STOP 83
    IF (KIND(ICHAR(C=CC(I8), KIND=KIND(K8)))  .NE. 8)                ERROR STOP 82
    IF (     ICHAR(C=CC(I8), KIND=KIND(K4))   .NE. I8)               ERROR STOP 83
    IF (KIND(ICHAR(C=CC(I8), KIND=KIND(K4)))  .NE. 4)                ERROR STOP 84
    IF (     ICHAR(C=CC(I8), KIND=KIND(K2))   .NE. I8)               ERROR STOP 85
    IF (KIND(ICHAR(C=CC(I8), KIND=KIND(K2)))  .NE. 2)                ERROR STOP 86
    IF (     ICHAR(C=CC(I8), KIND=KIND(K1))   .NE. I8)               ERROR STOP 87
    IF (KIND(ICHAR(C=CC(I8), KIND=KIND(K1)))  .NE. 1)                ERROR STOP 88
  END DO



  IF (ANY( ICHAR(C=CC, KIND=KIND(K8))   .NE. II))           ERROR STOP 11
  IF (KIND(ICHAR(C=CC, KIND=KIND(K8)))  .NE. 8)             ERROR STOP 12
  IF (ANY( ICHAR(C=CC, KIND=KIND(K4))   .NE. II))           ERROR STOP 13
  IF (KIND(ICHAR(C=CC, KIND=KIND(K4)))  .NE. 4)             ERROR STOP 14
  IF (ANY( ICHAR(C=CC, KIND=KIND(K2))   .NE. II))           ERROR STOP 15
  IF (KIND(ICHAR(C=CC, KIND=KIND(K2)))  .NE. 2)             ERROR STOP 16
  IF (ANY( ICHAR(C=CC, KIND=KIND(K1))   .NE. II))           ERROR STOP 17
  IF (KIND(ICHAR(C=CC, KIND=KIND(K1)))  .NE. 1)             ERROR STOP 18




  END
