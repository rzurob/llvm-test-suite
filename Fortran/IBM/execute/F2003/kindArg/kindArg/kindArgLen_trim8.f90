!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen_trim8
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 23, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : LEN_TRIM 
!*
!*  REFERENCE                  : Feature Number 289083 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!*   
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
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr1)))   .NE. I))   STOP 11
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr1))))  .NE. 1)    STOP 12
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr2)))   .NE. I))   STOP 13
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr2))))  .NE. 2)    STOP 14
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr4)))   .NE. I))   STOP 15
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr4))))  .NE. 4)    STOP 16
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr8)))   .NE. I))   STOP 17
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(:I), KIND=SUM(UBOUND(Arr8))))  .NE. 8)    STOP 18
  END DO

  DO I =1, 128
    IF (ANY( LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr1)))  .NE. 0))   STOP 21
    IF (KIND(LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr1)))) .NE. 1)     STOP 22
    IF (ANY( LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr2)))  .NE. 0))   STOP 23
    IF (KIND(LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr2)))) .NE. 2)     STOP 24
    IF (ANY( LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr4)))  .NE. 0))   STOP 25
    IF (KIND(LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr4)))) .NE. 4)     STOP 26
    IF (ANY( LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr8)))  .NE. 0))   STOP 27
    IF (KIND(LEN_TRIM(STRING=CC(I:)(I+1:I), KIND=SUM(UBOUND(Arr8)))) .NE. 8)     STOP 28
  END DO

  DO I = 1, 128
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr1)))  .NE. 0)) STOP 41
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr1)))) .NE. 1)  STOP 42
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr2)))  .NE. 0)) STOP 43
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr2)))) .NE. 2)  STOP 44
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr4)))  .NE. 0)) STOP 45
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr4)))) .NE. 4)  STOP 46
    IF (ANY( LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr8)))  .NE. 0)) STOP 47
    IF (KIND(LEN_TRIM(STRING=CC(I:I-1)(I+1:I), KIND=SUM(UBOUND(Arr8)))) .NE. 8)  STOP 48
  END DO

  DO I = 1, 128
    IF (ANY( LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr1)))  .NE. 1)) STOP 81
    IF (KIND(LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr1)))) .NE. 1)  STOP 82
    IF (ANY( LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr2)))  .NE. 1)) STOP 83
    IF (KIND(LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr2)))) .NE. 2)  STOP 84
    IF (ANY( LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr4)))  .NE. 1)) STOP 85
    IF (KIND(LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr4)))) .NE. 4)  STOP 86
    IF (ANY( LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr8)))  .NE. 1)) STOP 87
    IF (KIND(LEN_TRIM(STRING=CC(:)(I:I), KIND=SUM(UBOUND(Arr8)))) .NE. 8)  STOP 88
  END DO


  END

