!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : LEN
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
!*  Entities with different attribute used for kind arg - return from len
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen7
  IMPLICIT NONE

  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8


  CHARACTER(128) :: C(128)


  DO I2 = 1, 128
    IF (     LEN(STRING=C(I2:)(:), KIND=LEN(C(I2:)(1:1)))  .NE. 128)   STOP 21
    IF (KIND(LEN(STRING=C(I2:)(:), KIND=LEN(C(I2:)(1:1)))) .NE. 1)     STOP 22
    IF (     LEN(STRING=C(I2:)(:), KIND=LEN(C(I2:)(1:2)))  .NE. 128)   STOP 23
    IF (KIND(LEN(STRING=C(I2:)(:), KIND=LEN(C(I2:)(1:2)))) .NE. 2)     STOP 24
    IF (     LEN(STRING=C(I2:)(:), KIND=LEN(C(I2:)(1:4)))  .NE. 128)   STOP 25
    IF (KIND(LEN(STRING=C(I2:)(:), KIND=LEN(C(I2:)(1:4)))) .NE. 4)     STOP 26
    IF (     LEN(STRING=C(I2:)(:), KIND=LEN(C(I2:)(1:8)))  .NE. 128)   STOP 27
    IF (KIND(LEN(STRING=C(I2:)(:), KIND=LEN(C(I2:)(1:8)))) .NE. 8)     STOP 28
  END DO

  DO I4 = 1, 128
    IF (     LEN(STRING=C(:I4)(I4:I4), KIND=LEN(C(:I4)(1:1)))  .NE. 1)  STOP 41
    IF (KIND(LEN(STRING=C(:I4)(I4:I4), KIND=LEN(C(:I4)(1:1)))) .NE. 1)  STOP 42
    IF (     LEN(STRING=C(:I4)(I4:I4), KIND=LEN(C(:I4)(1:2)))  .NE. 1)  STOP 43
    IF (KIND(LEN(STRING=C(:I4)(I4:I4), KIND=LEN(C(:I4)(1:2)))) .NE. 2)  STOP 44
    IF (     LEN(STRING=C(:I4)(I4:I4), KIND=LEN(C(:I4)(1:4)))  .NE. 1)  STOP 45
    IF (KIND(LEN(STRING=C(:I4)(I4:I4), KIND=LEN(C(:I4)(1:4)))) .NE. 4)  STOP 46
    IF (     LEN(STRING=C(:I4)(I4:I4), KIND=LEN(C(:I4)(1:8)))  .NE. 1)  STOP 47
    IF (KIND(LEN(STRING=C(:I4)(I4:I4), KIND=LEN(C(:I4)(1:8)))) .NE. 8)  STOP 48
  END DO

  DO I8 = 1, 128
    IF (     LEN(STRING=C(I8+1:I8)(:), KIND=LEN(C(I8+1:I8)(1:1)))  .NE. 128)  STOP 81
    IF (KIND(LEN(STRING=C(I8+1:I8)(:), KIND=LEN(C(I8+1:I8)(1:1)))) .NE. 1)    STOP 82
    IF (     LEN(STRING=C(I8+1:I8)(:), KIND=LEN(C(I8+1:I8)(1:2)))  .NE. 128)  STOP 83
    IF (KIND(LEN(STRING=C(I8+1:I8)(:), KIND=LEN(C(I8+1:I8)(1:2)))) .NE. 2)    STOP 84
    IF (     LEN(STRING=C(I8+1:I8)(:), KIND=LEN(C(I8+1:I8)(1:4)))  .NE. 128)  STOP 85
    IF (KIND(LEN(STRING=C(I8+1:I8)(:), KIND=LEN(C(I8+1:I8)(1:4)))) .NE. 4)    STOP 86
    IF (     LEN(STRING=C(I8+1:I8)(:), KIND=LEN(C(I8+1:I8)(1:8)))  .NE. 128)  STOP 87
    IF (KIND(LEN(STRING=C(I8+1:I8)(:), KIND=LEN(C(I8+1:I8)(1:8)))) .NE. 8)    STOP 88
  END DO



  END

