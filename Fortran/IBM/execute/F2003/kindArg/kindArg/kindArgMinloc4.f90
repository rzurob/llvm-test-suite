!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgMinloc4
!*
!*  DATE                       : Jun. 27, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : MINLOC
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
!*  Description. Determine the location of the first element of ARRAY along dimension DIM
!*  having the minimum value of the elements identified by MASK.
!*  - REAL
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMinloc4


  REAL,    PARAMETER  :: R4(128:128+127)  = (/( I, I=127, 0, -1) /)
  REAL(8), PARAMETER  :: R8(128:128+127) =  (/( I, I=127, 0, -1) /)
  REAL(16),PARAMETER  :: R6(128:128+127) =  (/( I, I=127, 0, -1) /)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  LOGICAL      :: MM(128)=.TRUE.
  REAL         :: R(128,128)= 1.1


  DO I1 = 1, 126
    IF (ANY(MINLOC(ARRAY=R4(127+I1:128+126), MASK=MM(i1:127), KIND=1_1 ) .NE. 128-I1)) STOP 11
    IF (ANY(MINLOC(ARRAY=R4(127+I1:128+126), MASK=MM(i1:127), KIND=1_2 ) .NE. 128-I1)) STOP 12
    IF (ANY(MINLOC(ARRAY=R4(127+I1:128+126), MASK=MM(i1:127), KIND=1_4 ) .NE. 128-I1)) STOP 13
    IF (ANY(MINLOC(ARRAY=R4(127+I1:128+126), MASK=MM(i1:127), KIND=1_8 ) .NE. 128-I1)) STOP 14
  END DO

  DO I2 = 1, 128
    IF (ANY(MINLOC(ARRAY=R8(:127+I2), MASK=.TRUE._8, KIND=2_1 ) .NE. I2)) STOP 21
    IF (ANY(MINLOC(ARRAY=R8(:127+I2), MASK=.TRUE._4, KIND=2_2 ) .NE. I2)) STOP 22
    IF (ANY(MINLOC(ARRAY=R8(:127+I2), MASK=.TRUE._2, KIND=2_4 ) .NE. I2)) STOP 23
    IF (ANY(MINLOC(ARRAY=R8(:127+I2), MASK=.TRUE._1, KIND=2_8 ) .NE. I2)) STOP 24
  END DO

  DO I4 =1, 128
    IF (ANY(MINLOC(ARRAY=R6(128:127+I4), MASK= R4(128:127+I4) == R4(128:127+I4), KIND=4_1 ) .NE. I4)) STOP 41
    IF (ANY(MINLOC(ARRAY=R6(128:127+I4), MASK= R4(128:127+I4) == R4(128:127+I4), KIND=4_2 ) .NE. I4)) STOP 42
    IF (ANY(MINLOC(ARRAY=R6(128:127+I4), MASK= R4(128:127+I4) == R4(128:127+I4), KIND=4_4 ) .NE. I4)) STOP 43
    IF (ANY(MINLOC(ARRAY=R6(128:127+I4), MASK= R4(128:127+I4) == R4(128:127+I4), KIND=4_8 ) .NE. I4)) STOP 44
  END DO

  IF (ANY(MINLOC(ARRAY=R4(:), MASK=.FALSE._1, KIND=8_1 ) .NE. 0)) STOP 81
  IF (ANY(MINLOC(ARRAY=R4(:), MASK=.FALSE._2, KIND=8_2 ) .NE. 0)) STOP 82
  IF (ANY(MINLOC(ARRAY=R4(:), MASK=.FALSE._4, KIND=8_4 ) .NE. 0)) STOP 83
  IF (ANY(MINLOC(ARRAY=R4(:), MASK=.FALSE._8, KIND=8_8 ) .NE. 0)) STOP 84

  IF (ANY(MINLOC(ARRAY=R, DIM=1, KIND=2_1 ) .NE. 1)) STOP 91
  IF (ANY(MINLOC(ARRAY=R, DIM=2, KIND=4_4 ) .NE. 1))  STOP 92


  END

