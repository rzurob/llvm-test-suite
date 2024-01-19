!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 06, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : VERIFY
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
!*  Verify that a set of characters contains all the characters in a string by identifying the position
!*  of the first character in a string of characters that does not appear in a given set of characters.
!*
!*  (322447/323882)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgVerify4


  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  CHARACTER(:), POINTER :: Str(:)
  CHARACTER(:), ALLOCATABLE :: CC(:)


  ALLOCATE(CHARACTER(2) ::CC(127) )
  CC = (/(ACHAR(I=I, KIND=1)//ACHAR(I=I, KIND=1) , I=1, 127)/)

  ALLOCATE(CHARACTER(3) :: Str(127) )
  Str = (/( ACHAR(I) // CHAR(0) // ACHAR(I), I=1, 127)/)


  DO I1 = 1, 127
    IF (VERIFY(STRING=Str(I1), SET=CC(I1), BACK=.TRUE._8, KIND=1_1 ) .NE. 2) ERROR STOP 11
    IF (VERIFY(STRING=Str(I1), SET=CC(I1), BACK=.TRUE._4, KIND=1_2 ) .NE. 2) ERROR STOP 12
    IF (VERIFY(STRING=Str(I1), SET=CC(I1), BACK=.TRUE._2, KIND=1_4 ) .NE. 2) ERROR STOP 13
    IF (VERIFY(STRING=Str(I1), SET=CC(I1), BACK=.TRUE._1, KIND=1_8 ) .NE. 2) ERROR STOP 14
  END DO

  DO I2 = 1, 127
    IF (VERIFY(STRING=Str(I2), SET=CC(I2), BACK=.TRUE._1, KIND=2_1 ) .NE. 2) ERROR STOP 21
    IF (VERIFY(STRING=Str(I2), SET=CC(I2), BACK=.TRUE._2, KIND=2_2 ) .NE. 2) ERROR STOP 22
    IF (VERIFY(STRING=Str(I2), SET=CC(I2), BACK=.TRUE._4, KIND=2_4 ) .NE. 2) ERROR STOP 23
    IF (VERIFY(STRING=Str(I2), SET=CC(I2), BACK=.TRUE._8, KIND=2_8 ) .NE. 2) ERROR STOP 24
  END DO

  DO I4 =1, 127
    IF (VERIFY(STRING=Str(I4), SET=CC(I4), BACK=.TRUE._8, KIND=4_1 ) .NE. 2) ERROR STOP 41
    IF (VERIFY(STRING=Str(I4), SET=CC(I4), BACK=.TRUE._4, KIND=4_2 ) .NE. 2) ERROR STOP 42
    IF (VERIFY(STRING=Str(I4), SET=CC(I4), BACK=.TRUE._2, KIND=4_4 ) .NE. 2) ERROR STOP 43
    IF (VERIFY(STRING=Str(I4), SET=CC(I4), BACK=.TRUE._1, KIND=4_8 ) .NE. 2) ERROR STOP 44
  END DO

  DO I8 = 1, 127
    IF (VERIFY(STRING=Str(I8), SET=CC(I8), BACK=.TRUE._4, KIND=8_1 ) .NE. 2) ERROR STOP 81
    IF (VERIFY(STRING=Str(I8), SET=CC(I8), BACK=.TRUE._1, KIND=8_2 ) .NE. 2) ERROR STOP 82
    IF (VERIFY(STRING=Str(I8), SET=CC(I8), BACK=.TRUE._2, KIND=8_4 ) .NE. 2) ERROR STOP 83
    IF (VERIFY(STRING=Str(I8), SET=CC(I8), BACK=.TRUE._8, KIND=8_8 ) .NE. 2) ERROR STOP 84
  END DO

  IF (ANY(VERIFY(STRING=Str, SET=CC, BACK=.TRUE., KIND=1_1 ) .NE. 2)) ERROR STOP 91
  IF (ANY(VERIFY(STRING=Str, SET=CC)                         .NE. 2)) ERROR STOP 92

  IF (ANY( VERIFY(STRING=Str, SET=CC, BACK=.FALSE._1, KIND=1_8) .NE. VERIFY(STRING=Str, SET=CC))) ERROR STOP 111
  IF (ANY( VERIFY(STRING=Str, SET=CC, BACK=.FALSE._2, KIND=1_4) .NE. VERIFY(STRING=Str, SET=CC))) ERROR STOP 112
  IF (ANY( VERIFY(STRING=Str, SET=CC, BACK=.FALSE._4, KIND=1_2) .NE. VERIFY(STRING=Str, SET=CC))) ERROR STOP 113
  IF (ANY( VERIFY(STRING=Str, SET=CC, BACK=.FALSE._8, KIND=1_1) .NE. VERIFY(STRING=Str, SET=CC))) ERROR STOP 114

  END

