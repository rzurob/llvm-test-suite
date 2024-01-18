!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgScan3
!*
!*  DATE                       : Jun. 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SCAN
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
!*  Description. Scan a string for any one of the characters in a set of characters.
!*
!*  (322781/323955)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgScan3

  CHARACTER(*), PARAMETER  :: Set(127) = (/( ACHAR(I), I=1,127) /)
  CHARACTER(*), PARAMETER  :: Str(127) = (/( ACHAR(0)//ACHAR(I)//ACHAR(0), I=1,127) /)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8


  DO I1 = 1, 127
    IF (SCAN(STRING=Str(I1), SET=Set(I1), KIND=1_1 ) .NE. 2) STOP 11
    IF (SCAN(STRING=Str(I1), SET=Set(I1), KIND=1_2 ) .NE. 2) STOP 12
    IF (SCAN(STRING=Str(I1), SET=Set(I1), KIND=1_4 ) .NE. 2) STOP 13
    IF (SCAN(STRING=Str(I1), SET=Set(I1), KIND=1_8 ) .NE. 2) STOP 14
  END DO

  DO I2 = 1, 127
    IF (SCAN(STRING=Str(I2), SET=Set(I2), KIND=2_1 ) .NE. 2) STOP 21
    IF (SCAN(STRING=Str(I2), SET=Set(I2), KIND=2_2 ) .NE. 2) STOP 22
    IF (SCAN(STRING=Str(I2), SET=Set(I2), KIND=2_4 ) .NE. 2) STOP 23
    IF (SCAN(STRING=Str(I2), SET=Set(I2), KIND=2_8 ) .NE. 2) STOP 24
  END DO

  DO I4 = 1, 127
    IF (SCAN(STRING=Str(I4), SET=Set(I4), KIND=4_1 ) .NE. 2) STOP 41
    IF (SCAN(STRING=Str(I4), SET=Set(I4), KIND=4_2 ) .NE. 2) STOP 42
    IF (SCAN(STRING=Str(I4), SET=Set(I4), KIND=4_4 ) .NE. 2) STOP 43
    IF (SCAN(STRING=Str(I4), SET=Set(I4), KIND=4_8 ) .NE. 2) STOP 44
  END DO

  DO I8 = 1, 127
    IF (SCAN(STRING=Str(I8), SET=Set(I8), KIND=8_1 ) .NE. 2) STOP 81
    IF (SCAN(STRING=Str(I8), SET=Set(I8), KIND=8_2 ) .NE. 2) STOP 82
    IF (SCAN(STRING=Str(I8), SET=Set(I8), KIND=8_4 ) .NE. 2) STOP 83
    IF (SCAN(STRING=Str(I8), SET=Set(I8), KIND=8_8 ) .NE. 2) STOP 84
  END DO

  IF (ANY(SCAN(STRING=Str, SET=Set, KIND=1_1 ) .NE. 2)) STOP 91
  IF (ANY(SCAN(STRING=Str, SET=Set)            .NE. 2)) STOP 92


  DO I1 = 1, 127
    IF (SCAN(STRING=Str(I1), SET=Set(I1), BACK=.TRUE., KIND=1_1 ) .NE. 2) STOP 111
    IF (SCAN(STRING=Str(I1), SET=Set(I1), BACK=.TRUE., KIND=1_2 ) .NE. 2) STOP 112
    IF (SCAN(STRING=Str(I1), SET=Set(I1), BACK=.TRUE., KIND=1_4 ) .NE. 2) STOP 113
    IF (SCAN(STRING=Str(I1), SET=Set(I1), BACK=.TRUE., KIND=1_8 ) .NE. 2) STOP 114
  END DO

  DO I2 = 1, 127
    IF (SCAN(STRING=Str(I2), SET=Set(I2), BACK=.TRUE., KIND=2_1 ) .NE. 2) STOP 121
    IF (SCAN(STRING=Str(I2), SET=Set(I2), BACK=.TRUE., KIND=2_2 ) .NE. 2) STOP 122
    IF (SCAN(STRING=Str(I2), SET=Set(I2), BACK=.TRUE., KIND=2_4 ) .NE. 2) STOP 123
    IF (SCAN(STRING=Str(I2), SET=Set(I2), BACK=.TRUE., KIND=2_8 ) .NE. 2) STOP 124
  END DO

  DO I4 = 1, 127
    IF (SCAN(STRING=Str(I4), SET=Set(I4), BACK=.TRUE., KIND=4_1 ) .NE. 2) STOP 141
    IF (SCAN(STRING=Str(I4), SET=Set(I4), BACK=.TRUE., KIND=4_2 ) .NE. 2) STOP 142
    IF (SCAN(STRING=Str(I4), SET=Set(I4), BACK=.TRUE., KIND=4_4 ) .NE. 2) STOP 143
    IF (SCAN(STRING=Str(I4), SET=Set(I4), BACK=.TRUE., KIND=4_8 ) .NE. 2) STOP 144
  END DO

  DO I8 = 1, 127
    IF (SCAN(STRING=Str(I8), SET=Set(I8), BACK=.TRUE., KIND=8_1 ) .NE. 2) STOP 181
    IF (SCAN(STRING=Str(I8), SET=Set(I8), BACK=.TRUE., KIND=8_2 ) .NE. 2) STOP 182
    IF (SCAN(STRING=Str(I8), SET=Set(I8), BACK=.TRUE., KIND=8_4 ) .NE. 2) STOP 183
    IF (SCAN(STRING=Str(I8), SET=Set(I8), BACK=.TRUE., KIND=8_8 ) .NE. 2) STOP 184
  END DO

  IF (ANY(SCAN(STRING=Str, SET=Set, BACK=.TRUE., KIND=1_1 ) .NE. 2)) STOP 191
  IF (ANY(SCAN(STRING=Str, SET=Set, BACK=.TRUE.)            .NE. 2)) STOP 192



  END

