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
!*  (322430)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgVerify3

  CHARACTER(*), PARAMETER  :: Str(127) = (/CHARACTER(3) :: ( ACHAR(I)//"  ", I=1,127) /)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  CHARACTER(2)    :: CC(1:127) = (/(ACHAR(0)//ACHAR(0), I=1,127)/)
  INTEGER         :: II(1:127) = (/(I, I=1,127)/)

  DO I1 = 1, 127
    IF (VERIFY(STRING=Str(I1), SET=CC(I1), KIND=1_1 ) .NE. 1) ERROR STOP 11
    IF (VERIFY(STRING=Str(I1), SET=CC(I1), KIND=1_2 ) .NE. 1) ERROR STOP 12
    IF (VERIFY(STRING=Str(I1), SET=CC(I1), KIND=1_4 ) .NE. 1) ERROR STOP 13
    IF (VERIFY(STRING=Str(I1), SET=CC(I1), KIND=1_8 ) .NE. 1) ERROR STOP 14
  END DO

  DO I2 = 1, 127
    IF (VERIFY(STRING=Str(I2)(:), SET=CC(I2), KIND=2_1 ) .NE. 1) ERROR STOP 21
    IF (VERIFY(STRING=Str(I2)(:), SET=CC(I2), KIND=2_2 ) .NE. 1) ERROR STOP 22
    IF (VERIFY(STRING=Str(I2)(:), SET=CC(I2), KIND=2_4 ) .NE. 1) ERROR STOP 23
    IF (VERIFY(STRING=Str(I2)(:), SET=CC(I2), KIND=2_8 ) .NE. 1) ERROR STOP 24
  END DO

  DO I4 =1, 127
    IF (VERIFY(STRING=Str(I4)(:2), SET=CC(I4), KIND=4_1 ) .NE. 1) ERROR STOP 41
    IF (VERIFY(STRING=Str(I4)(:2), SET=CC(I4), KIND=4_2 ) .NE. 1) ERROR STOP 42
    IF (VERIFY(STRING=Str(I4)(:2), SET=CC(I4), KIND=4_4 ) .NE. 1) ERROR STOP 43
    IF (VERIFY(STRING=Str(I4)(:2), SET=CC(I4), KIND=4_8 ) .NE. 1) ERROR STOP 44
  END DO

  DO I8 = 1, 127
    IF (VERIFY(STRING=Str(I8)(1:), SET=CC(I8), KIND=8_1 ) .NE. 1) ERROR STOP 81
    IF (VERIFY(STRING=Str(I8)(1:), SET=CC(I8), KIND=8_2 ) .NE. 1) ERROR STOP 82
    IF (VERIFY(STRING=Str(I8)(1:), SET=CC(I8), KIND=8_4 ) .NE. 1) ERROR STOP 83
    IF (VERIFY(STRING=Str(I8)(1:), SET=CC(I8), KIND=8_8 ) .NE. 1) ERROR STOP 84
  END DO

  IF (ANY(VERIFY(STRING=Str, SET=CC, KIND=1_1 ) .NE. 1)) ERROR STOP 91
  IF (ANY(VERIFY(STRING=Str, SET=CC)            .NE. 1)) ERROR STOP 92

  IF (ANY(VERIFY(STRING=Str, SET=CHAR(I=0,KIND=1_8), KIND=1_1 ) .NE. 1)) ERROR STOP 93
  IF (ANY(VERIFY(STRING=Str, SET=CHAR(I=0,KIND=1_2))            .NE. 1)) ERROR STOP 94


  END
