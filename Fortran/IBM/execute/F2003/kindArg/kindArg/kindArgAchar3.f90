!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgAchar3
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : ACHAR 
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
!*  Returns the character in a specified position of the ASCII collating sequence. 
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgAchar3

  CHARACTER, ALLOCATABLE :: C(:)

  !CHARACTER    :: Ascii(95) = (/  &
  !" ", "!", """", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/",      & 
  !"0", "1",  "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?", "@", &
  !"A", "B", "C",  "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", &
  !"R", "S", "T",  "U", "V", "W", "X", "Y", "Z",                                         &
  !"[", "\\", "]",  "^", "_", "`",                                                        &
  !"a", "b", "c",  "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", & 
  !"r", "s", "t",  "u", "v", "w", "x", "y", "z",                                         &
  !"{", "|",  "}", "~" /)

  CHARACTER    :: Ascii(32:126) = (/  &
  " ", &
  "!", &
  """",&
  "#", &
  "$", &
  "%", &
  "&", &
  "'", &
  "(", &
  ")", &
  "*", &
  "+", &
  ",", &
  "-", &
  ".", &
  "/", &
  "0", &
  "1", &
  "2", &
  "3", &
  "4", &
  "5", &
  "6", &
  "7", &
  "8", &
  "9", &
  ":", &
  ";", &
  "<", &
  "=", &
  ">", &
  "?", &
  "@", &
  "A", &
  "B", &
  "C", &
  "D", &
  "E", &
  "F", &
  "G", &
  "H", &
  "I", &
  "J", &
  "K", &
  "L", &
  "M", &
  "N", &
  "O", &
  "P", &
  "Q", &
  "R", &
  "S", &
  "T", &
  "U", &
  "V", &
  "W", &
  "X", &
  "Y", &
  "Z", &
  "[", &
  "\\",&
  "]", &
  "^", &
  "_", &
  "`", &
  "a", &
  "b", &
  "c", &
  "d", &
  "e", &
  "f", &
  "g", &
  "h", &
  "i", &
  "j", &
  "k", &
  "l", &
  "m", &
  "n", &
  "o", &
  "p", &
  "q", &
  "r", &
  "s", &
  "t", &
  "u", &
  "v", &
  "w", &
  "x", &
  "y", &
  "z", &
  "{", &
  "|", &
  "}", &
  "~" /)
  
  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8
     
  ALLOCATE(C(95), SOURCE=Ascii)

  DO I1 =32, 126
    IF (ACHAR(I1, KIND=1_1 ) .NE. Ascii(I1)) STOP 11
    IF (ACHAR(I1, KIND=1_2 ) .NE. Ascii(I1)) STOP 12
    IF (ACHAR(I1, KIND=1_4 ) .NE. Ascii(I1)) STOP 13
    IF (ACHAR(I1, KIND=1_8 ) .NE. Ascii(I1)) STOP 14
  END DO

  DO I2 =32, 126
    IF (ACHAR(I2, KIND=1_1 ) .NE. Ascii(I2)) STOP 21
    IF (ACHAR(I2, KIND=1_2 ) .NE. Ascii(I2)) STOP 22
    IF (ACHAR(I2, KIND=1_4 ) .NE. Ascii(I2)) STOP 23
    IF (ACHAR(I2, KIND=1_8 ) .NE. Ascii(I2)) STOP 24
  END DO

  DO I4 =32, 126
    IF (ACHAR(I4, KIND=1_1 ) .NE. Ascii(I4)) STOP 31
    IF (ACHAR(I4, KIND=1_2 ) .NE. Ascii(I4)) STOP 32
    IF (ACHAR(I4, KIND=1_4 ) .NE. Ascii(I4)) STOP 33
    IF (ACHAR(I4, KIND=1_8 ) .NE. Ascii(I4)) STOP 34
  END DO

  DO I8 =32, 126
    IF (ACHAR(I8, KIND=1_1 ) .NE. Ascii(I8)) STOP 41
    IF (ACHAR(I8, KIND=1_2 ) .NE. Ascii(I8)) STOP 42
    IF (ACHAR(I8, KIND=1_4 ) .NE. Ascii(I8)) STOP 43
    IF (ACHAR(I8, KIND=1_8 ) .NE. Ascii(I8)) STOP 44
  END DO

  DEALLOCATE(C)

  END

