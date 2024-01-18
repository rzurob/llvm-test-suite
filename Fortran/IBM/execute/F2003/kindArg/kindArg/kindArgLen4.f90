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
!*  Description. Returns the length of a character entity.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen4


  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  CHARACTER(127), PARAMETER :: CC(0:127)=" "
  INTEGER, PARAMETER :: KIND(4)=(/1,2,4,8/)



  DO I1 = 0, 127
    IF (LEN(STRING=CC(I1)(:I1), KIND=KIND(4) )    .NE. I1)                   STOP 11
    IF (LEN(STRING=CC(I1)(:I1), KIND=KIND(3) )    .NE. I1)                   STOP 12
    IF (LEN(STRING=CC(I1)(:I1), KIND=KIND(2) )    .NE. I1)                   STOP 13
    IF (LEN(STRING=CC(I1)(:I1), KIND=KIND(1) )    .NE. I1)                   STOP 14
  END DO

  DO I2 = 0, 127
    IF (LEN(STRING=CC(I2:)(:), KIND=KIND(4) )    .NE. 127)                   STOP 21
    IF (LEN(STRING=CC(I2:)(:), KIND=KIND(3) )    .NE. 127)                   STOP 22
    IF (LEN(STRING=CC(I2:)(:), KIND=KIND(2) )    .NE. 127)                   STOP 23
    IF (LEN(STRING=CC(I2:)(:), KIND=KIND(1) )    .NE. 127)                   STOP 24
  END DO

  DO I4 = 0, 127
    IF (LEN(STRING=CC(:I4)(I4+1:I4+1), KIND=KIND(4) )    .NE. 1)             STOP 31
    IF (LEN(STRING=CC(:I4)(I4+1:I4+1), KIND=KIND(3) )    .NE. 1)             STOP 32
    IF (LEN(STRING=CC(:I4)(I4+1:I4+1), KIND=KIND(2) )    .NE. 1)             STOP 33
    IF (LEN(STRING=CC(:I4)(I4+1:I4+1), KIND=KIND(1) )    .NE. 1)             STOP 34
  END DO

  DO I8 = 1, 127
    IF (LEN(STRING=CC(I8+1:I8)(:), KIND=KIND(4) )    .NE. 127)               STOP 41
    IF (LEN(STRING=CC(I8+1:I8)(:), KIND=KIND(3) )    .NE. 127)               STOP 42
    IF (LEN(STRING=CC(I8+1:I8)(:), KIND=KIND(2) )    .NE. 127)               STOP 43
    IF (LEN(STRING=CC(I8+1:I8)(:), KIND=KIND(1) )    .NE. 127)               STOP 44
  END DO


  END

