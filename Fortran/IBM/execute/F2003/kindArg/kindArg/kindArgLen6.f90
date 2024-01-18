!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 22, 2006
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
!*  Entities with different attubute used for kind arg - associate/select type
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen6


  TYPE :: DT
    INTEGER(1) :: K1(1:1)=1
    INTEGER(2) :: K2(2:3)=2
    INTEGER(4) :: K4(4:7)=4
    INTEGER(8) :: K8(8:15)=8
  END TYPE

  CLASS(DT), POINTER     :: TT(:)
  CLASS(*),  ALLOCATABLE :: CC(:)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8


  ALLOCATE(CHARACTER(127) :: CC(0:127))
  ALLOCATE(TT(128))


  ASSOCIATE (T=>TT(1))
  SELECT TYPE (C => CC)
  TYPE IS (CHARACTER(*))

  DO I2 = 0, 127
    IF (     LEN(STRING=C(I2:)(:), KIND=KIND(T%K1))  .NE. 127)   STOP 21
    IF (KIND(LEN(STRING=C(I2:)(:), KIND=KIND(T%K1))) .NE. 1)     STOP 22
    IF (     LEN(STRING=C(I2:)(:), KIND=KIND(T%K2))  .NE. 127)   STOP 23
    IF (KIND(LEN(STRING=C(I2:)(:), KIND=KIND(T%K2))) .NE. 2)     STOP 24
    IF (     LEN(STRING=C(I2:)(:), KIND=KIND(T%K4))  .NE. 127)   STOP 25
    IF (KIND(LEN(STRING=C(I2:)(:), KIND=KIND(T%K4))) .NE. 4)     STOP 26
    IF (     LEN(STRING=C(I2:)(:), KIND=KIND(T%K8))  .NE. 127)   STOP 27
    IF (KIND(LEN(STRING=C(I2:)(:), KIND=KIND(T%K8))) .NE. 8)     STOP 28
  END DO

  DO I4 = 0, 128
    IF (     LEN(STRING=C(:I4)(I4:I4), KIND=KIND(T%K1))  .NE. 1)  STOP 41
    IF (KIND(LEN(STRING=C(:I4)(I4:I4), KIND=KIND(T%K1))) .NE. 1)  STOP 42
    IF (     LEN(STRING=C(:I4)(I4:I4), KIND=KIND(T%K2))  .NE. 1)  STOP 43
    IF (KIND(LEN(STRING=C(:I4)(I4:I4), KIND=KIND(T%K2))) .NE. 2)  STOP 44
    IF (     LEN(STRING=C(:I4)(I4:I4), KIND=KIND(T%K4))  .NE. 1)  STOP 45
    IF (KIND(LEN(STRING=C(:I4)(I4:I4), KIND=KIND(T%K4))) .NE. 4)  STOP 46
    IF (     LEN(STRING=C(:I4)(I4:I4), KIND=KIND(T%K8))  .NE. 1)  STOP 47
    IF (KIND(LEN(STRING=C(:I4)(I4:I4), KIND=KIND(T%K8))) .NE. 8)  STOP 48
  END DO

  DO I8 = 0, 128
    IF (     LEN(STRING=C(I8+1:I8)(:), KIND=KIND(T%K1))  .NE. 127)  STOP 81
    IF (KIND(LEN(STRING=C(I8+1:I8)(:), KIND=KIND(T%K1))) .NE. 1)    STOP 82
    IF (     LEN(STRING=C(I8+1:I8)(:), KIND=KIND(T%K2))  .NE. 127)  STOP 83
    IF (KIND(LEN(STRING=C(I8+1:I8)(:), KIND=KIND(T%K2))) .NE. 2)    STOP 84
    IF (     LEN(STRING=C(I8+1:I8)(:), KIND=KIND(T%K4))  .NE. 127)  STOP 85
    IF (KIND(LEN(STRING=C(I8+1:I8)(:), KIND=KIND(T%K4))) .NE. 4)    STOP 86
    IF (     LEN(STRING=C(I8+1:I8)(:), KIND=KIND(T%K8))  .NE. 127)  STOP 87
    IF (KIND(LEN(STRING=C(I8+1:I8)(:), KIND=KIND(T%K8))) .NE. 8)    STOP 88
  END DO


  CLASS DEFAULT
    STOP 96
  END SELECT
  END ASSOCIATE


  END

