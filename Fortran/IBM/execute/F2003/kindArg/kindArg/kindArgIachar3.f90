!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIachar3
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : IACHAR 
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
!*  Returns the position of a character in the ASCII collating sequence. 
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIachar3

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
    IF (IACHAR(C=Ascii(I1), KIND=1_1 ) .NE. I1) STOP 11
    IF (IACHAR(C=Ascii(I1), KIND=1_2 ) .NE. I1) STOP 12
    IF (IACHAR(C=Ascii(I1), KIND=1_4 ) .NE. I1) STOP 13
    IF (IACHAR(C=Ascii(I1), KIND=1_8 ) .NE. I1) STOP 14
  END DO

  DO I2 =32, 126
    IF (IACHAR(C=Ascii(I2), KIND=1_1 ) .NE. I2) STOP 21
    IF (IACHAR(C=Ascii(I2), KIND=1_2 ) .NE. I2) STOP 22
    IF (IACHAR(C=Ascii(I2), KIND=1_4 ) .NE. I2) STOP 23
    IF (IACHAR(C=Ascii(I2), KIND=1_8 ) .NE. I2) STOP 24
  END DO

  DO I4 =32, 126
    IF (IACHAR(C=Ascii(I4), KIND=1_1 ) .NE. I4) STOP 41
    IF (IACHAR(C=Ascii(I4), KIND=1_2 ) .NE. I4) STOP 42
    IF (IACHAR(C=Ascii(I4), KIND=1_4 ) .NE. I4) STOP 43
    IF (IACHAR(C=Ascii(I4), KIND=1_8 ) .NE. I4) STOP 44
  END DO

  DO I8 =32, 126
    IF (IACHAR(C=Ascii(I8), KIND=1_1 ) .NE. I8) STOP 81
    IF (IACHAR(C=Ascii(I8), KIND=1_2 ) .NE. I8) STOP 82
    IF (IACHAR(C=Ascii(I8), KIND=1_4 ) .NE. I8) STOP 83
    IF (IACHAR(C=Ascii(I8), KIND=1_8 ) .NE. I8) STOP 84
  END DO

  IF (ANY(IACHAR(C=CC, KIND=1_1 ) .NE. II)) STOP 91
  IF (ANY(IACHAR(C=CC)            .NE. II)) STOP 92


  END

