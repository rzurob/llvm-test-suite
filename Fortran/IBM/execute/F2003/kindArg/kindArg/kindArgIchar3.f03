!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ICHAR
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
!*  Returns the position of a character in the processor collating sequence associated
!*  with the kind type parameter of the character. This is the inverse of the CHAR function
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIchar3

  CHARACTER    :: Ascii(32:126) = (/                                                        &
  " ", "!", """", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/",      &
  "0", "1",  "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?", "@", &
  "A", "B", "C",  "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", &
  "R", "S", "T",  "U", "V", "W", "X", "Y", "Z",                                         &
  "[", "\\", "]",  "^", "_", "`",                                                       &
  "a", "b", "c",  "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", &
  "r", "s", "t",  "u", "v", "w", "x", "y", "z",                                         &
  "{", "|",  "}", "~" /)


  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  CHARACTER    :: CC(128) = (/(ACHAR(I), I=0,127)/)
  INTEGER      :: II(128) = (/(I, I=0,127)/)

  DO I1 =32, 126
    IF (ICHAR(C=Ascii(I1), KIND=1_1 ) .NE. I1) ERROR STOP 11
    IF (ICHAR(C=Ascii(I1), KIND=1_2 ) .NE. I1) ERROR STOP 12
    IF (ICHAR(C=Ascii(I1), KIND=1_4 ) .NE. I1) ERROR STOP 13
    IF (ICHAR(C=Ascii(I1), KIND=1_8 ) .NE. I1) ERROR STOP 14
  END DO

  DO I2 =32, 126
    IF (ICHAR(C=Ascii(I2), KIND=1_1 ) .NE. I2) ERROR STOP 21
    IF (ICHAR(C=Ascii(I2), KIND=1_2 ) .NE. I2) ERROR STOP 22
    IF (ICHAR(C=Ascii(I2), KIND=1_4 ) .NE. I2) ERROR STOP 23
    IF (ICHAR(C=Ascii(I2), KIND=1_8 ) .NE. I2) ERROR STOP 24
  END DO

  DO I4 =32, 126
    IF (ICHAR(C=Ascii(I4), KIND=1_1 ) .NE. I4) ERROR STOP 41
    IF (ICHAR(C=Ascii(I4), KIND=1_2 ) .NE. I4) ERROR STOP 42
    IF (ICHAR(C=Ascii(I4), KIND=1_4 ) .NE. I4) ERROR STOP 43
    IF (ICHAR(C=Ascii(I4), KIND=1_8 ) .NE. I4) ERROR STOP 44
  END DO

  DO I8 =32, 126
    IF (ICHAR(C=Ascii(I8), KIND=1_1 ) .NE. I8) ERROR STOP 81
    IF (ICHAR(C=Ascii(I8), KIND=1_2 ) .NE. I8) ERROR STOP 82
    IF (ICHAR(C=Ascii(I8), KIND=1_4 ) .NE. I8) ERROR STOP 83
    IF (ICHAR(C=Ascii(I8), KIND=1_8 ) .NE. I8) ERROR STOP 84
  END DO

  IF (ANY(ICHAR(C=CC, KIND=1_1 ) .NE. II)) ERROR STOP 91
  IF (ANY(ICHAR(C=CC)            .NE. II)) ERROR STOP 92

  IF (ANY(ICHAR(C=CHAR(I=II,KIND=1_1), KIND=1_1 ) .NE. II)) ERROR STOP 93
  IF (ANY(ICHAR(C=CHAR(I=II,KIND=1_1))            .NE. II)) ERROR STOP 94


  END

