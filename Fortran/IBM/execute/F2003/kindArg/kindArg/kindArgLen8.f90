!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen8
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : LEN 
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


  PROGRAM kindArgLen8
  IMPLICIT NONE


  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8


  CHARACTER(128), PARAMETER :: C(128)=""


  DO I2 = 0, 128
    IF (     LEN(STRING=C(I2:I2-1)(:), KIND=LEN(C(2:2-1)(1:1)))  .NE. 128)   STOP 21
    IF (KIND(LEN(STRING=C(I2:I2-1)(:), KIND=LEN(C(2:2-1)(1:1)))) .NE. 1)     STOP 22
    IF (     LEN(STRING=C(I2:I2-1)(:), KIND=LEN(C(2:2-1)(1:2)))  .NE. 128)   STOP 23
    IF (KIND(LEN(STRING=C(I2:I2-1)(:), KIND=LEN(C(2:2-1)(1:2)))) .NE. 2)     STOP 24
    IF (     LEN(STRING=C(I2:I2-1)(:), KIND=LEN(C(2:2-1)(1:4)))  .NE. 128)   STOP 25
    IF (KIND(LEN(STRING=C(I2:I2-1)(:), KIND=LEN(C(2:2-1)(1:4)))) .NE. 4)     STOP 26
    IF (     LEN(STRING=C(I2:I2-1)(:), KIND=LEN(C(2:2-1)(1:8)))  .NE. 128)   STOP 27
    IF (KIND(LEN(STRING=C(I2:I2-1)(:), KIND=LEN(C(2:2-1)(1:8)))) .NE. 8)     STOP 28
  END DO

  DO I4 = 0, 128
    IF (     LEN(STRING=C(:I4)(I4:I4-1), KIND=LEN(C(:4)(1:1)))  .NE. 0)  STOP 41
    IF (KIND(LEN(STRING=C(:I4)(I4:I4-1), KIND=LEN(C(:4)(1:1)))) .NE. 1)  STOP 42
    IF (     LEN(STRING=C(:I4)(I4:I4-1), KIND=LEN(C(:4)(1:2)))  .NE. 0)  STOP 43
    IF (KIND(LEN(STRING=C(:I4)(I4:I4-1), KIND=LEN(C(:4)(1:2)))) .NE. 2)  STOP 44
    IF (     LEN(STRING=C(:I4)(I4:I4-1), KIND=LEN(C(:4)(1:4)))  .NE. 0)  STOP 45
    IF (KIND(LEN(STRING=C(:I4)(I4:I4-1), KIND=LEN(C(:4)(1:4)))) .NE. 4)  STOP 46
    IF (     LEN(STRING=C(:I4)(I4:I4-1), KIND=LEN(C(:4)(1:8)))  .NE. 0)  STOP 47
    IF (KIND(LEN(STRING=C(:I4)(I4:I4-1), KIND=LEN(C(:4)(1:8)))) .NE. 8)  STOP 48
  END DO

  DO I8 = 0, 128
    IF (     LEN(STRING=C(I8:I8-1)(1:0), KIND=LEN(C(8+1:8)(1:1)))  .NE. 0 )   STOP 81
    IF (KIND(LEN(STRING=C(I8:I8-1)(1:0), KIND=LEN(C(8+1:8)(1:1)))) .NE. 1)    STOP 82
    IF (     LEN(STRING=C(I8:I8-1)(1:0), KIND=LEN(C(8+1:8)(1:2)))  .NE. 0 )   STOP 83
    IF (KIND(LEN(STRING=C(I8:I8-1)(1:0), KIND=LEN(C(8+1:8)(1:2)))) .NE. 2)    STOP 84
    IF (     LEN(STRING=C(I8:I8-1)(1:0), KIND=LEN(C(8+1:8)(1:4)))  .NE. 0 )   STOP 85
    IF (KIND(LEN(STRING=C(I8:I8-1)(1:0), KIND=LEN(C(8+1:8)(1:4)))) .NE. 4)    STOP 86
    IF (     LEN(STRING=C(I8:I8-1)(1:0), KIND=LEN(C(8+1:8)(1:8)))  .NE. 0 )   STOP 87
    IF (KIND(LEN(STRING=C(I8:I8-1)(1:0), KIND=LEN(C(8+1:8)(1:8)))) .NE. 8)    STOP 88
  END DO



  END

