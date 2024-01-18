!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen_trim6
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 22, 2006
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
!*  Entities with different attubute used for kind arg - associate/select type 
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen_trim6
  IMPLICIT NONE

  CLASS(*),  ALLOCATABLE :: CC(:)

  INTEGER                 :: I
  CHARACTER(*), PARAMETER :: TT(128)="12345678"
  INTEGER,      PARAMETER :: I1=1,I2=2,I4=4,I8=8

  ALLOCATE( CC(128), SOURCE="1234567890123456        ")


  SELECT TYPE (CC)
  TYPE IS (CHARACTER(*))


  DO I = 1, 16
    IF (     LEN_TRIM(STRING=CC(I)(:I), KIND=LEN_TRIM(TT(I1)(I1:I1)))   .NE. I )   STOP 11
    IF (KIND(LEN_TRIM(STRING=CC(I)(:I), KIND=LEN_TRIM(TT(I1)(I1:I1))))  .NE. 1)    STOP 12
    IF (     LEN_TRIM(STRING=CC(I)(:I), KIND=LEN_TRIM(TT(I1)(I1:I2)))   .NE. I )   STOP 13
    IF (KIND(LEN_TRIM(STRING=CC(I)(:I), KIND=LEN_TRIM(TT(I1)(I1:I2))))  .NE. 2)    STOP 14
    IF (     LEN_TRIM(STRING=CC(I)(:I), KIND=LEN_TRIM(TT(I1)(I1:I4)))   .NE. I )   STOP 15
    IF (KIND(LEN_TRIM(STRING=CC(I)(:I), KIND=LEN_TRIM(TT(I1)(I1:I4))))  .NE. 4)    STOP 16
    IF (     LEN_TRIM(STRING=CC(I)(:I), KIND=LEN_TRIM(TT(I1)(I1:I8)))   .NE. I )   STOP 17
    IF (KIND(LEN_TRIM(STRING=CC(I)(:I), KIND=LEN_TRIM(TT(I1)(I1:I8))))  .NE. 8)    STOP 18
  END DO

  DO I =1, 16
    IF (ANY( LEN_TRIM(STRING=CC(I:)(:), KIND=LEN_TRIM(TT(I2)(I1:I1)))  .NE. 16))   STOP 21
    IF (KIND(LEN_TRIM(STRING=CC(I:)(:), KIND=LEN_TRIM(TT(I2)(I1:I1)))) .NE. 1)     STOP 22
    IF (ANY( LEN_TRIM(STRING=CC(I:)(:), KIND=LEN_TRIM(TT(I2)(I1:I2)))  .NE. 16))   STOP 23
    IF (KIND(LEN_TRIM(STRING=CC(I:)(:), KIND=LEN_TRIM(TT(I2)(I1:I2)))) .NE. 2)     STOP 24
    IF (ANY( LEN_TRIM(STRING=CC(I:)(:), KIND=LEN_TRIM(TT(I2)(I1:I4)))  .NE. 16))   STOP 25
    IF (KIND(LEN_TRIM(STRING=CC(I:)(:), KIND=LEN_TRIM(TT(I2)(I1:I4)))) .NE. 4)     STOP 26
    IF (ANY( LEN_TRIM(STRING=CC(I:)(:), KIND=LEN_TRIM(TT(I2)(I1:I8)))  .NE. 16))   STOP 27
    IF (KIND(LEN_TRIM(STRING=CC(I:)(:), KIND=LEN_TRIM(TT(I2)(I1:I8)))) .NE. 8)     STOP 28
  END DO

  DO I = 1, 16
    IF (ANY( LEN_TRIM(STRING=CC(:I)(I:I), KIND=LEN_TRIM(TT(I4)(I1:I1)))  .NE. 1))  STOP 41
    IF (KIND(LEN_TRIM(STRING=CC(:I)(I:I), KIND=LEN_TRIM(TT(I4)(I1:I1)))) .NE. 1)   STOP 42
    IF (ANY( LEN_TRIM(STRING=CC(:I)(I:I), KIND=LEN_TRIM(TT(I4)(I1:I2)))  .NE. 1))  STOP 43
    IF (KIND(LEN_TRIM(STRING=CC(:I)(I:I), KIND=LEN_TRIM(TT(I4)(I1:I2)))) .NE. 2)   STOP 44
    IF (ANY( LEN_TRIM(STRING=CC(:I)(I:I), KIND=LEN_TRIM(TT(I4)(I1:I4)))  .NE. 1))  STOP 45
    IF (KIND(LEN_TRIM(STRING=CC(:I)(I:I), KIND=LEN_TRIM(TT(I4)(I1:I4)))) .NE. 4)   STOP 46
    IF (ANY( LEN_TRIM(STRING=CC(:I)(I:I), KIND=LEN_TRIM(TT(I4)(I1:I8)))  .NE. 1))  STOP 47
    IF (KIND(LEN_TRIM(STRING=CC(:I)(I:I), KIND=LEN_TRIM(TT(I4)(I1:I8)))) .NE. 8)   STOP 48
  END DO

  DO I = 1, 16
    IF (ANY( LEN_TRIM(STRING=CC(:)(I+1:I), KIND=LEN_TRIM(TT(I8)(I1:I1)))  .NE. 0)) STOP 81
    IF (KIND(LEN_TRIM(STRING=CC(:)(I+1:I), KIND=LEN_TRIM(TT(I8)(I1:I1)))) .NE. 1)  STOP 82
    IF (ANY( LEN_TRIM(STRING=CC(:)(I+1:I), KIND=LEN_TRIM(TT(I8)(I1:I2)))  .NE. 0)) STOP 83
    IF (KIND(LEN_TRIM(STRING=CC(:)(I+1:I), KIND=LEN_TRIM(TT(I8)(I1:I2)))) .NE. 2)  STOP 84
    IF (ANY( LEN_TRIM(STRING=CC(:)(I+1:I), KIND=LEN_TRIM(TT(I8)(I1:I4)))  .NE. 0)) STOP 85
    IF (KIND(LEN_TRIM(STRING=CC(:)(I+1:I), KIND=LEN_TRIM(TT(I8)(I1:I4)))) .NE. 4)  STOP 86
    IF (ANY( LEN_TRIM(STRING=CC(:)(I+1:I), KIND=LEN_TRIM(TT(I8)(I1:I8)))  .NE. 0)) STOP 87
    IF (KIND(LEN_TRIM(STRING=CC(:)(I+1:I), KIND=LEN_TRIM(TT(I8)(I1:I8)))) .NE. 8)  STOP 88
  END DO


  CLASS DEFAULT
    STOP 96
  END SELECT 


  END

