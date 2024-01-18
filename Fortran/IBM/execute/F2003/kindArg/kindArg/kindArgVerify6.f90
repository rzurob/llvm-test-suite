!*********************************************************************
!*  ===================================================================
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
!*  Result Value.
!*  Case (i): If BACK is absent or has the value false and if STRING contains at least one
!*  character that is not in SET, the value of the result is the position of the leftmost
!*  character of STRING that is not in SET.
!*
!*  The length set is zero
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgVerify6
  IMPLICIT NONE


  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4, I
  INTEGER(8) :: I8

  INTEGER                     :: II(128)=(/(I,I=0,127)/)

  CHARACTER(128) :: C= ""
  CHARACTER(128) :: CC(0:127)= ""


  DO I =0, 127
    CC(I)(I+1:I+1)= CHAR(I)
  END DO

  DO I1 = 0, 127
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), KIND=LEN(C(1:8)))   .NE. 1)               ERROR STOP 11
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), KIND=LEN(C(1:8))))  .NE. 8)               ERROR STOP 12
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), KIND=LEN(C(1:4)))   .NE. 1)               ERROR STOP 13
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), KIND=LEN(C(1:4))))  .NE. 4)               ERROR STOP 14
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), KIND=LEN(C(1:2)))   .NE. 1)               ERROR STOP 15
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), KIND=LEN(C(1:2))))  .NE. 2)               ERROR STOP 16
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), KIND=LEN(C(1:1)))   .NE. 1)               ERROR STOP 17
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), KIND=LEN(C(1:1))))  .NE. 1)               ERROR STOP 18
  END DO

  DO I2 = 0, 127
    IF (     VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), KIND=LEN(C(1:8)))   .NE. 1)           ERROR STOP 21
    IF (KIND(VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), KIND=LEN(C(1:8))))  .NE. 8)           ERROR STOP 22
    IF (     VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), KIND=LEN(C(1:4)))   .NE. 1)           ERROR STOP 23
    IF (KIND(VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), KIND=LEN(C(1:4))))  .NE. 4)           ERROR STOP 24
    IF (     VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), KIND=LEN(C(1:2)))   .NE. 1)           ERROR STOP 25
    IF (KIND(VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), KIND=LEN(C(1:2))))  .NE. 2)           ERROR STOP 26
    IF (     VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), KIND=LEN(C(1:1)))   .NE. 1)           ERROR STOP 27
    IF (KIND(VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), KIND=LEN(C(1:1))))  .NE. 1)           ERROR STOP 28
  END DO

  DO I4 = 0, 127
    IF (     VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), KIND=LEN(C(1:8)))   .NE. 1)          ERROR STOP 31
    IF (KIND(VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), KIND=LEN(C(1:8))))  .NE. 8)          ERROR STOP 32
    IF (     VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), KIND=LEN(C(1:4)))   .NE. 1)          ERROR STOP 33
    IF (KIND(VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), KIND=LEN(C(1:4))))  .NE. 4)          ERROR STOP 34
    IF (     VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), KIND=LEN(C(1:2)))   .NE. 1)          ERROR STOP 35
    IF (KIND(VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), KIND=LEN(C(1:2))))  .NE. 2)          ERROR STOP 36
    IF (     VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), KIND=LEN(C(1:1)))   .NE. 1)          ERROR STOP 37
    IF (KIND(VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), KIND=LEN(C(1:1))))  .NE. 1)          ERROR STOP 38
  END DO

  DO I8 = 0, 127
    IF (     VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+8:I8), KIND=LEN(C(1:8)))   .NE. 1)          ERROR STOP 43
    IF (KIND(VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+8:I8), KIND=LEN(C(1:8))))  .NE. 8)          ERROR STOP 42
    IF (     VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+8:I8), KIND=LEN(C(1:4)))   .NE. 1)          ERROR STOP 43
    IF (KIND(VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+8:I8), KIND=LEN(C(1:4))))  .NE. 4)          ERROR STOP 44
    IF (     VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+8:I8), KIND=LEN(C(1:2)))   .NE. 1)          ERROR STOP 45
    IF (KIND(VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+8:I8), KIND=LEN(C(1:2))))  .NE. 2)          ERROR STOP 46
    IF (     VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+8:I8), KIND=LEN(C(1:1)))   .NE. 1)          ERROR STOP 47
    IF (KIND(VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+8:I8), KIND=LEN(C(1:1))))  .NE. 1)          ERROR STOP 48
  END DO


  DO I1 = 0, 127
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), BACK=.FALSE., KIND=LEN(C(1:8)))   .NE. 1)               ERROR STOP 51
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), BACK=.FALSE., KIND=LEN(C(1:8))))  .NE. 8)               ERROR STOP 52
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), BACK=.FALSE., KIND=LEN(C(1:4)))   .NE. 1)               ERROR STOP 53
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), BACK=.FALSE., KIND=LEN(C(1:4))))  .NE. 4)               ERROR STOP 54
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), BACK=.FALSE., KIND=LEN(C(1:2)))   .NE. 1)               ERROR STOP 55
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), BACK=.FALSE., KIND=LEN(C(1:2))))  .NE. 2)               ERROR STOP 56
    IF (     VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), BACK=.FALSE., KIND=LEN(C(1:1)))   .NE. 1)               ERROR STOP 57
    IF (KIND(VERIFY(STRING=CC(I1), SET=CC(I1)(I1+1:I1), BACK=.FALSE., KIND=LEN(C(1:1))))  .NE. 1)               ERROR STOP 58
  END DO

  DO I2 = 0, 127
    IF (     VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), BACK=.FALSE._1, KIND=LEN(C(1:8)))   .NE. 1)         ERROR STOP 61
    IF (KIND(VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), BACK=.FALSE._1, KIND=LEN(C(1:8))))  .NE. 8)         ERROR STOP 62
    IF (     VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), BACK=.FALSE._1, KIND=LEN(C(1:4)))   .NE. 1)         ERROR STOP 63
    IF (KIND(VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), BACK=.FALSE._1, KIND=LEN(C(1:4))))  .NE. 4)         ERROR STOP 64
    IF (     VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), BACK=.FALSE._1, KIND=LEN(C(1:2)))   .NE. 1)         ERROR STOP 65
    IF (KIND(VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), BACK=.FALSE._1, KIND=LEN(C(1:2))))  .NE. 2)         ERROR STOP 66
    IF (     VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), BACK=.FALSE._1, KIND=LEN(C(1:1)))   .NE. 1)         ERROR STOP 67
    IF (KIND(VERIFY(STRING=CC(I2)(2:), SET=CC(I2)(I2+1:I2), BACK=.FALSE._1, KIND=LEN(C(1:1))))  .NE. 1)         ERROR STOP 68
  END DO

  DO I4 = 0, 127
    IF (     VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), BACK=.FALSE._2, KIND=LEN(C(1:8)))   .NE. 1)        ERROR STOP 71
    IF (KIND(VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), BACK=.FALSE._2, KIND=LEN(C(1:8))))  .NE. 8)        ERROR STOP 72
    IF (     VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), BACK=.FALSE._2, KIND=LEN(C(1:4)))   .NE. 1)        ERROR STOP 73
    IF (KIND(VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), BACK=.FALSE._2, KIND=LEN(C(1:4))))  .NE. 4)        ERROR STOP 74
    IF (     VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), BACK=.FALSE._2, KIND=LEN(C(1:2)))   .NE. 1)        ERROR STOP 75
    IF (KIND(VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), BACK=.FALSE._2, KIND=LEN(C(1:2))))  .NE. 2)        ERROR STOP 76
    IF (     VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), BACK=.FALSE._2, KIND=LEN(C(1:1)))   .NE. 1)        ERROR STOP 77
    IF (KIND(VERIFY(STRING=CC(I4)(3:3), SET=CC(I4)(I4+1:I4), BACK=.FALSE._2, KIND=LEN(C(1:1))))  .NE. 1)        ERROR STOP 78
  END DO

  DO I8 = 0, 127
    IF (     VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+1:I8), BACK=.FALSE._8, KIND=LEN(C(1:8)))   .NE. 1)        ERROR STOP 81
    IF (KIND(VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+1:I8), BACK=.FALSE._8, KIND=LEN(C(1:8))))  .NE. 8)        ERROR STOP 82
    IF (     VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+1:I8), BACK=.FALSE._8, KIND=LEN(C(1:4)))   .NE. 1)        ERROR STOP 83
    IF (KIND(VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+1:I8), BACK=.FALSE._8, KIND=LEN(C(1:4))))  .NE. 4)        ERROR STOP 84
    IF (     VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+1:I8), BACK=.FALSE._8, KIND=LEN(C(1:2)))   .NE. 1)        ERROR STOP 85
    IF (KIND(VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+1:I8), BACK=.FALSE._8, KIND=LEN(C(1:2))))  .NE. 2)        ERROR STOP 86
    IF (     VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+1:I8), BACK=.FALSE._8, KIND=LEN(C(1:1)))   .NE. 1)        ERROR STOP 87
    IF (KIND(VERIFY(STRING=CC(I8)(1:3), SET=CC(I8)(I8+1:I8), BACK=.FALSE._8, KIND=LEN(C(1:1))))  .NE. 1)        ERROR STOP 88
  END DO



  IF (ANY( VERIFY(STRING=CC, SET="", KIND=LEN(C(1:8)))   .NE. 1))           ERROR STOP 91
  IF (KIND(VERIFY(STRING=CC, SET="", KIND=LEN(C(1:8))))  .NE. 8)            ERROR STOP 92
  IF (ANY( VERIFY(STRING=CC, SET="", KIND=LEN(C(1:4)))   .NE. 1))           ERROR STOP 93
  IF (KIND(VERIFY(STRING=CC, SET="", KIND=LEN(C(1:4))))  .NE. 4)            ERROR STOP 94
  IF (ANY( VERIFY(STRING=CC, SET="", KIND=LEN(C(1:2)))   .NE. 1))           ERROR STOP 95
  IF (KIND(VERIFY(STRING=CC, SET="", KIND=LEN(C(1:2))))  .NE. 2)            ERROR STOP 96
  IF (ANY( VERIFY(STRING=CC, SET="", KIND=LEN(C(1:1)))   .NE. 1))           ERROR STOP 97
  IF (KIND(VERIFY(STRING=CC, SET="", KIND=LEN(C(1:1))))  .NE. 1)            ERROR STOP 98


  IF (ANY( VERIFY(STRING=CC, SET="", BACK=.FALSE._8, KIND=LEN(C(1:8)))   .NE. 1))           ERROR STOP 111
  IF (KIND(VERIFY(STRING=CC, SET="", BACK=.FALSE._8, KIND=LEN(C(1:8))))  .NE. 8)            ERROR STOP 112
  IF (ANY( VERIFY(STRING=CC, SET="", BACK=.FALSE._8, KIND=LEN(C(1:4)))   .NE. 1))           ERROR STOP 113
  IF (KIND(VERIFY(STRING=CC, SET="", BACK=.FALSE._8, KIND=LEN(C(1:4))))  .NE. 4)            ERROR STOP 114
  IF (ANY( VERIFY(STRING=CC, SET="", BACK=.FALSE._8, KIND=LEN(C(1:2)))   .NE. 1))           ERROR STOP 115
  IF (KIND(VERIFY(STRING=CC, SET="", BACK=.FALSE._8, KIND=LEN(C(1:2))))  .NE. 2)            ERROR STOP 116
  IF (ANY( VERIFY(STRING=CC, SET="", BACK=.FALSE._8, KIND=LEN(C(1:1)))   .NE. 1))           ERROR STOP 117
  IF (KIND(VERIFY(STRING=CC, SET="", BACK=.FALSE._8, KIND=LEN(C(1:1))))  .NE. 1)            ERROR STOP 118


  END


