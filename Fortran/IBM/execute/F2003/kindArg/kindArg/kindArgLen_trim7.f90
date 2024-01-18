!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen_trim7
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
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
!*  Entities with different attribute used for kind arg - return from len 
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen_trim7
  IMPLICIT NONE

  INTEGER    :: I
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  INTEGER :: Arr1(1), Arr2(2), Arr4(4), Arr8(8)

  CHARACTER(:), ALLOCATABLE :: CC(:)

  CALL IntSub(CC)
 
  DO I = 1, 127 
    IF (     LEN_TRIM(STRING=CC(I)(:I), KIND=SUM(SHAPE(Arr1)))   .NE. 0)   STOP 11
    IF (KIND(LEN_TRIM(STRING=CC(I)(:I), KIND=SUM(SHAPE(Arr1))))  .NE. 1)    STOP 12
    IF (     LEN_TRIM(STRING=CC(I)(:I), KIND=SUM(SHAPE(Arr2)))   .NE. 0)   STOP 13
    IF (KIND(LEN_TRIM(STRING=CC(I)(:I), KIND=SUM(SHAPE(Arr2))))  .NE. 2)    STOP 14
    IF (     LEN_TRIM(STRING=CC(I)(:I), KIND=SUM(SHAPE(Arr4)))   .NE. 0)   STOP 15
    IF (KIND(LEN_TRIM(STRING=CC(I)(:I), KIND=SUM(SHAPE(Arr4))))  .NE. 4)    STOP 16
    IF (     LEN_TRIM(STRING=CC(I)(:I), KIND=SUM(SHAPE(Arr8)))   .NE. 0)   STOP 17
    IF (KIND(LEN_TRIM(STRING=CC(I)(:I), KIND=SUM(SHAPE(Arr8))))  .NE. 8)    STOP 18
  END DO

  DO I =1, 128 
    IF (ANY( LEN_TRIM(STRING=CC(I:)(:), KIND=SUM(SHAPE(Arr1)))  .NE. 0))   STOP 21
    IF (KIND(LEN_TRIM(STRING=CC(I:)(:), KIND=SUM(SHAPE(Arr1)))) .NE. 1)     STOP 22
    IF (ANY( LEN_TRIM(STRING=CC(I:)(:), KIND=SUM(SHAPE(Arr2)))  .NE. 0))   STOP 23
    IF (KIND(LEN_TRIM(STRING=CC(I:)(:), KIND=SUM(SHAPE(Arr2)))) .NE. 2)     STOP 24
    IF (ANY( LEN_TRIM(STRING=CC(I:)(:), KIND=SUM(SHAPE(Arr4)))  .NE. 0))   STOP 25
    IF (KIND(LEN_TRIM(STRING=CC(I:)(:), KIND=SUM(SHAPE(Arr4)))) .NE. 4)     STOP 26
    IF (ANY( LEN_TRIM(STRING=CC(I:)(:), KIND=SUM(SHAPE(Arr8)))  .NE. 0))   STOP 27
    IF (KIND(LEN_TRIM(STRING=CC(I:)(:), KIND=SUM(SHAPE(Arr8)))) .NE. 8)     STOP 28
  END DO

  DO I = 1, 128 
    IF (ANY( LEN_TRIM(STRING=CC(:I)(I:I), KIND=SUM(SHAPE(Arr1)))  .NE. 0)) STOP 41
    IF (KIND(LEN_TRIM(STRING=CC(:I)(I:I), KIND=SUM(SHAPE(Arr1)))) .NE. 1)  STOP 42
    IF (ANY( LEN_TRIM(STRING=CC(:I)(I:I), KIND=SUM(SHAPE(Arr2)))  .NE. 0)) STOP 43
    IF (KIND(LEN_TRIM(STRING=CC(:I)(I:I), KIND=SUM(SHAPE(Arr2)))) .NE. 2)  STOP 44
    IF (ANY( LEN_TRIM(STRING=CC(:I)(I:I), KIND=SUM(SHAPE(Arr4)))  .NE. 0)) STOP 45
    IF (KIND(LEN_TRIM(STRING=CC(:I)(I:I), KIND=SUM(SHAPE(Arr4)))) .NE. 4)  STOP 46
    IF (ANY( LEN_TRIM(STRING=CC(:I)(I:I), KIND=SUM(SHAPE(Arr8)))  .NE. 0)) STOP 47
    IF (KIND(LEN_TRIM(STRING=CC(:I)(I:I), KIND=SUM(SHAPE(Arr8)))) .NE. 8)  STOP 48
  END DO

  DO I = 1, 128 
    IF (ANY( LEN_TRIM(STRING=CC(:)(I+1:I), KIND=SUM(SHAPE(Arr1)))  .NE. 0)) STOP 81
    IF (KIND(LEN_TRIM(STRING=CC(:)(I+1:I), KIND=SUM(SHAPE(Arr1)))) .NE. 1)  STOP 82
    IF (ANY( LEN_TRIM(STRING=CC(:)(I+1:I), KIND=SUM(SHAPE(Arr2)))  .NE. 0)) STOP 83
    IF (KIND(LEN_TRIM(STRING=CC(:)(I+1:I), KIND=SUM(SHAPE(Arr2)))) .NE. 2)  STOP 84
    IF (ANY( LEN_TRIM(STRING=CC(:)(I+1:I), KIND=SUM(SHAPE(Arr4)))  .NE. 0)) STOP 85
    IF (KIND(LEN_TRIM(STRING=CC(:)(I+1:I), KIND=SUM(SHAPE(Arr4)))) .NE. 4)  STOP 86
    IF (ANY( LEN_TRIM(STRING=CC(:)(I+1:I), KIND=SUM(SHAPE(Arr8)))  .NE. 0)) STOP 87
    IF (KIND(LEN_TRIM(STRING=CC(:)(I+1:I), KIND=SUM(SHAPE(Arr8)))) .NE. 8)  STOP 88
  END DO


  CONTAINS

  SUBROUTINE IntSub(Arg)
  CHARACTER(:), ALLOCATABLE :: Arg(:)
    ALLOCATE(CHARACTER(128) :: Arg(128))
    Arg(:)=""
  END SUBROUTINE

  END

