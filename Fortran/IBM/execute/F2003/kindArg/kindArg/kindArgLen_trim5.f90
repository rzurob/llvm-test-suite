!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 22, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : LEN_TRIM
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
!*    Integer. If KIND is present, the kind type parameter is that specified by the value of KIND;
!*    otherwise the kind type parameter is that of default integer type
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen_trim5
  IMPLICIT NONE

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  CHARACTER(32), PARAMETER :: CC(16)="1111111111111111"

  DO I1 = 1, 16
    IF (     LEN_TRIM(STRING=CC(I1)(:I1), KIND=LEN(CC(1)(1:1)))     .NE. I1)   STOP 11
    IF (KIND(LEN_TRIM(STRING=CC(I1)(:I1), KIND=LEN(CC(1)(1:1))))    .NE. 1)    STOP 12
    IF (     LEN_TRIM(STRING=CC(I1)(:I1), KIND=LEN(CC(1)(1:1))+1)   .NE. I1)   STOP 13
    IF (KIND(LEN_TRIM(STRING=CC(I1)(:I1), KIND=LEN(CC(1)(1:1))+1))  .NE. 2)    STOP 14
    IF (     LEN_TRIM(STRING=CC(I1)(:I1), KIND=LEN(CC(1)(1:1))+3)   .NE. I1)   STOP 15
    IF (KIND(LEN_TRIM(STRING=CC(I1)(:I1), KIND=LEN(CC(1)(1:1))+3))  .NE. 4)    STOP 16
    IF (     LEN_TRIM(STRING=CC(I1)(:I1), KIND=LEN(CC(1)(1:1))+7)   .NE. I1)   STOP 17
    IF (KIND(LEN_TRIM(STRING=CC(I1)(:I1), KIND=LEN(CC(1)(1:1))+7))  .NE. 8)    STOP 18
  END DO

  DO I2 =1, 16
    IF (ANY( LEN_TRIM(STRING=CC(I2:)(:), KIND=LEN(CC(2)(2:2)))    .NE. 16))   STOP 21
    IF (KIND(LEN_TRIM(STRING=CC(I2:)(:), KIND=LEN(CC(2)(2:2))))   .NE. 1)     STOP 22
    IF (ANY( LEN_TRIM(STRING=CC(I2:)(:), KIND=LEN(CC(2)(2:2))+1)  .NE. 16))   STOP 23
    IF (KIND(LEN_TRIM(STRING=CC(I2:)(:), KIND=LEN(CC(2)(2:2))+1)) .NE. 2)     STOP 24
    IF (ANY( LEN_TRIM(STRING=CC(I2:)(:), KIND=LEN(CC(2)(2:2))+3)  .NE. 16))   STOP 25
    IF (KIND(LEN_TRIM(STRING=CC(I2:)(:), KIND=LEN(CC(2)(2:2))+3)) .NE. 4)     STOP 26
    IF (ANY( LEN_TRIM(STRING=CC(I2:)(:), KIND=LEN(CC(2)(2:2))+7)  .NE. 16))   STOP 27
    IF (KIND(LEN_TRIM(STRING=CC(I2:)(:), KIND=LEN(CC(2)(2:2))+7)) .NE. 8)     STOP 28
  END DO

  DO I4 = 1, 16
    IF (ANY( LEN_TRIM(STRING=CC(:I4)(I4:I4), KIND=LEN_TRIM(CC(4)(4:4)))    .NE. 1)) STOP 41
    IF (KIND(LEN_TRIM(STRING=CC(:I4)(I4:I4), KIND=LEN_TRIM(CC(4)(4:4))))   .NE. 1)  STOP 42
    IF (ANY( LEN_TRIM(STRING=CC(:I4)(I4:I4), KIND=LEN_TRIM(CC(4)(4:4))+1)  .NE. 1)) STOP 43
    IF (KIND(LEN_TRIM(STRING=CC(:I4)(I4:I4), KIND=LEN_TRIM(CC(4)(4:4))+1)) .NE. 2)  STOP 44
    IF (ANY( LEN_TRIM(STRING=CC(:I4)(I4:I4), KIND=LEN_TRIM(CC(4)(4:4))+3)  .NE. 1)) STOP 45
    IF (KIND(LEN_TRIM(STRING=CC(:I4)(I4:I4), KIND=LEN_TRIM(CC(4)(4:4))+3)) .NE. 4)  STOP 46
    IF (ANY( LEN_TRIM(STRING=CC(:I4)(I4:I4), KIND=LEN_TRIM(CC(4)(4:4))+7)  .NE. 1)) STOP 47
    IF (KIND(LEN_TRIM(STRING=CC(:I4)(I4:I4), KIND=LEN_TRIM(CC(4)(4:4))+7)) .NE. 8)  STOP 48
  END DO

  DO I8 = 1, 16
    IF ( ANY(LEN_TRIM(STRING=CC(:)(I8+1:I8), KIND=LEN_TRIM(CC(8)(8:8)))    .NE. 0)) STOP 81
    IF (KIND(LEN_TRIM(STRING=CC(:)(I8+1:I8), KIND=LEN_TRIM(CC(8)(8:8))))   .NE. 1)  STOP 82
    IF ( ANY(LEN_TRIM(STRING=CC(:)(I8+1:I8), KIND=LEN_TRIM(CC(8)(8:8))+1)  .NE. 0)) STOP 83
    IF (KIND(LEN_TRIM(STRING=CC(:)(I8+1:I8), KIND=LEN_TRIM(CC(8)(8:8))+1)) .NE. 2)  STOP 84
    IF ( ANY(LEN_TRIM(STRING=CC(:)(I8+1:I8), KIND=LEN_TRIM(CC(8)(8:8))+3)  .NE. 0)) STOP 85
    IF (KIND(LEN_TRIM(STRING=CC(:)(I8+1:I8), KIND=LEN_TRIM(CC(8)(8:8))+3)) .NE. 4)  STOP 86
    IF ( ANY(LEN_TRIM(STRING=CC(:)(I8+1:I8), KIND=LEN_TRIM(CC(8)(8:8))+7)  .NE. 0)) STOP 87
    IF (KIND(LEN_TRIM(STRING=CC(:)(I8+1:I8), KIND=LEN_TRIM(CC(8)(8:8))+7)) .NE. 8)  STOP 88
  END DO


  END

