!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 23, 2006
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
!*  Length=0/size=0
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen_trim8
  IMPLICIT NONE

  INTEGER :: Arr1(1), Arr2(2), Arr4(4), Arr8(8)

  INTEGER :: I
  CHARACTER(128) :: CC(128)

  DO I=1, 128
    CC(:)(I:I) = "x"
  END DO


  DO I = 2, 127
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr1)))   .NE. I))   ERROR STOP 11
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr1))))  .NE. 1)    ERROR STOP 12
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr2)))   .NE. I))   ERROR STOP 13
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr2))))  .NE. 2)    ERROR STOP 14
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr4)))   .NE. I))   ERROR STOP 15
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr4))))  .NE. 4)    ERROR STOP 16
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr8)))   .NE. I))   ERROR STOP 17
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr8))))  .NE. 8)    ERROR STOP 18
  END DO

  DO I =1, 128
    IF (ANY( LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr1)))  .NE. 0))   ERROR STOP 21
    IF (KIND(LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr1)))) .NE. 1)     ERROR STOP 22
    IF (ANY( LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr2)))  .NE. 0))   ERROR STOP 23
    IF (KIND(LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr2)))) .NE. 2)     ERROR STOP 24
    IF (ANY( LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr4)))  .NE. 0))   ERROR STOP 25
    IF (KIND(LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr4)))) .NE. 4)     ERROR STOP 26
    IF (ANY( LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr8)))  .NE. 0))   ERROR STOP 27
    IF (KIND(LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr8)))) .NE. 8)     ERROR STOP 28
  END DO

  DO I = 1, 128
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr1)))  .NE. 0)) ERROR STOP 41
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr1)))) .NE. 1)  ERROR STOP 42
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr2)))  .NE. 0)) ERROR STOP 43
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr2)))) .NE. 2)  ERROR STOP 44
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr4)))  .NE. 0)) ERROR STOP 45
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr4)))) .NE. 4)  ERROR STOP 46
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr8)))  .NE. 0)) ERROR STOP 47
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr8)))) .NE. 8)  ERROR STOP 48
  END DO

  DO I = 1, 128
    IF (ANY( LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr1)))  .NE. 1)) ERROR STOP 81
    IF (KIND(LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr1)))) .NE. 1)  ERROR STOP 82
    IF (ANY( LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr2)))  .NE. 1)) ERROR STOP 83
    IF (KIND(LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr2)))) .NE. 2)  ERROR STOP 84
    IF (ANY( LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr4)))  .NE. 1)) ERROR STOP 85
    IF (KIND(LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr4)))) .NE. 4)  ERROR STOP 86
    IF (ANY( LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr8)))  .NE. 1)) ERROR STOP 87
    IF (KIND(LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr8)))) .NE. 8)  ERROR STOP 88
  END DO


  END
