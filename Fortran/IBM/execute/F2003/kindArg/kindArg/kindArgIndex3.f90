!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIndex3
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
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
!*  Returns the starting position of a substring within a string.
!*    
!*  (322620) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIndex3

  CHARACTER(*), PARAMETER  :: Str(0:127) = (/CHARACTER(3) :: ( ACHAR(I)//"  ", I=0,127) /)
  !CHARACTER(*), PARAMETER  :: Str(0:127) = (/ ( ACHAR(I)//"  ", I=0,127) /)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8
     
  CHARACTER    :: CC(0:127) = (/(ACHAR(I), I=0,127)/) 
  INTEGER      :: II(128) = (/(I, I=0,127)/)

  DO I1 = 0, 127
    IF (INDEX(STRING=Str(I1), SUBSTRING=CC(I1), KIND=1_1 ) .NE. 1) STOP 11
    IF (INDEX(STRING=Str(I1), SUBSTRING=CC(I1), KIND=1_2 ) .NE. 1) STOP 12
    IF (INDEX(STRING=Str(I1), SUBSTRING=CC(I1), KIND=1_4 ) .NE. 1) STOP 13
    IF (INDEX(STRING=Str(I1), SUBSTRING=CC(I1), KIND=1_8 ) .NE. 1) STOP 14
  END DO

  DO I2 = 0, 127
    IF (INDEX(STRING=Str(I2), SUBSTRING=CC(I2), KIND=2_1 ) .NE. 1) STOP 21
    IF (INDEX(STRING=Str(I2), SUBSTRING=CC(I2), KIND=2_2 ) .NE. 1) STOP 22
    IF (INDEX(STRING=Str(I2), SUBSTRING=CC(I2), KIND=2_4 ) .NE. 1) STOP 23
    IF (INDEX(STRING=Str(I2), SUBSTRING=CC(I2), KIND=2_8 ) .NE. 1) STOP 24
  END DO

  DO I4 =0, 127 
    IF (INDEX(STRING=Str(I4), SUBSTRING=CC(I4), KIND=4_1 ) .NE. 1) STOP 41
    IF (INDEX(STRING=Str(I4), SUBSTRING=CC(I4), KIND=4_2 ) .NE. 1) STOP 42
    IF (INDEX(STRING=Str(I4), SUBSTRING=CC(I4), KIND=4_4 ) .NE. 1) STOP 43
    IF (INDEX(STRING=Str(I4), SUBSTRING=CC(I4), KIND=4_8 ) .NE. 1) STOP 44
  END DO

  DO I8 = 0, 127
    IF (INDEX(STRING=Str(I8), SUBSTRING=CC(I8), KIND=8_1 ) .NE. 1) STOP 81
    IF (INDEX(STRING=Str(I8), SUBSTRING=CC(I8), KIND=8_2 ) .NE. 1) STOP 82
    IF (INDEX(STRING=Str(I8), SUBSTRING=CC(I8), KIND=8_4 ) .NE. 1) STOP 83
    IF (INDEX(STRING=Str(I8), SUBSTRING=CC(I8), KIND=8_8 ) .NE. 1) STOP 84
  END DO

  IF (ANY(INDEX(STRING=Str, SUBSTRING=CC, KIND=1_1 ) .NE. 1)) STOP 91
  IF (ANY(INDEX(STRING=Str, SUBSTRING=CC)            .NE. 1)) STOP 92

  IF (ANY(INDEX(STRING=Str, SUBSTRING=CHAR(I=II,KIND=1_8), KIND=1_1 ) .NE. 1)) STOP 93
  IF (ANY(INDEX(STRING=Str, SUBSTRING=CHAR(I=II,KIND=1_2))            .NE. 1)) STOP 94


  END

