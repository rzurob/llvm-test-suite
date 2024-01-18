!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgScan8
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 28, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : INDEX 
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
!*  Long string(128*128*8) 
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgScan8
  IMPLICIT NONE

  INTEGER(4) :: I4
  INTEGER(8) :: I8

  INTEGER   :: I, J
  CHARACTER(LEN=128*128*8) :: C 
  CHARACTER(LEN=128*128*8) :: CC(8) 
  

  DO I=1, LEN(C)
    C(I:I)= ACHAR(MOD(I,128)) 
    CC(:)(I:I)= ACHAR(MOD(I,128)) 
  END DO


  DO I4 = 1, 127
    IF (     SCAN(STRING=C, SET=C(I4:I4), KIND=KIND(8_8))   .NE. I4)               STOP 11
    IF (KIND(SCAN(STRING=C, SET=C(I4:I4), KIND=KIND(8_8)))  .NE. 8)                STOP 12
    IF (     SCAN(STRING=C, SET=C(I4:I4), KIND=KIND(4_4))   .NE. I4)               STOP 13
    IF (KIND(SCAN(STRING=C, SET=C(I4:I4), KIND=KIND(4_4)))  .NE. 4)                STOP 14
  END DO

  DO I4 = 1, 127
    IF (     SCAN(STRING=C, SET=C(I4:I4), BACK=.FALSE._4, KIND=KIND(8_8))   .NE. I4)               STOP 21
    IF (KIND(SCAN(STRING=C, SET=C(I4:I4), BACK=.FALSE._4, KIND=KIND(8_8)))  .NE. 8)                STOP 22
    IF (     SCAN(STRING=C, SET=C(I4:I4), BACK=.FALSE._8, KIND=KIND(4_4))   .NE. I4)               STOP 23
    IF (KIND(SCAN(STRING=C, SET=C(I4:I4), BACK=.FALSE._8, KIND=KIND(4_4)))  .NE. 4)                STOP 24
  END DO

  DO I8 = 1, 127
    IF (     SCAN(STRING=C, SET=C(I8:I8), BACK=.TRUE._1, KIND=KIND(8_8))   .NE. LEN(C)-(128-I8))   STOP 31
    IF (KIND(SCAN(STRING=C, SET=C(I8:I8), BACK=.TRUE._1, KIND=KIND(8_8)))  .NE. 8)                 STOP 32
    IF (     SCAN(STRING=C, SET=C(I8:I8), BACK=.TRUE._2, KIND=KIND(4_4))   .NE. LEN(C)-(128-I8))   STOP 33
    IF (KIND(SCAN(STRING=C, SET=C(I8:I8), BACK=.TRUE._2, KIND=KIND(4_4)))  .NE. 4)                 STOP 34
  END DO

  IF (ANY( SCAN(STRING=CC, SET=CC, KIND=KIND(8_8))   .NE. 1))           STOP 41
  IF (KIND(SCAN(STRING=CC, SET=CC, KIND=KIND(8_8)))  .NE. 8)            STOP 42
  IF (ANY( SCAN(STRING=CC, SET=CC, KIND=KIND(4_4))   .NE. 1))           STOP 43
  IF (KIND(SCAN(STRING=CC, SET=CC, KIND=KIND(4_4)))  .NE. 4)            STOP 44

  IF (ANY( SCAN(STRING=CC, SET=CC, BACK=.FALSE., KIND=KIND(8_8))   .NE. 1))         STOP 51
  IF (KIND(SCAN(STRING=CC, SET=CC, BACK=.FALSE., KIND=KIND(8_8)))  .NE. 8)          STOP 52
  IF (ANY( SCAN(STRING=CC, SET=CC, BACK=.FALSE., KIND=KIND(4_4))   .NE. 1))         STOP 53
  IF (KIND(SCAN(STRING=CC, SET=CC, BACK=.FALSE., KIND=KIND(4_4)))  .NE. 4)          STOP 54

  IF (ANY( SCAN(STRING=CC, SET=CC, BACK=.TRUE., KIND=KIND(8_8))   .NE. LEN(CC)))    STOP 61
  IF (KIND(SCAN(STRING=CC, SET=CC, BACK=.TRUE., KIND=KIND(8_8)))  .NE. 8)           STOP 62
  IF (ANY( SCAN(STRING=CC, SET=CC, BACK=.TRUE., KIND=KIND(4_4))   .NE. LEN(CC)))    STOP 63
  IF (KIND(SCAN(STRING=CC, SET=CC, BACK=.TRUE., KIND=KIND(4_4)))  .NE. 4)           STOP 64


  END

