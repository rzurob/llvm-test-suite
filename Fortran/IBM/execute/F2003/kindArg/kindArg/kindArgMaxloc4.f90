!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgMaxloc4
!*
!*  DATE                       : Jun. 26, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : MAXLOC
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
!*  Determine the location of the first element of ARRAY along dimension DIM
!*  having the maximum value of the elements identified by MASK.
!*  - REAL
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMaxloc4


  REAL,    PARAMETER  :: R4(128)  = (/( I, I=0,127) /)
  REAL(8), PARAMETER  :: R8(128) =  (/( I, I=0,127) /)
  REAL(16),PARAMETER  :: R6(128) =  (/( I, I=0,127) /)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  LOGICAL      :: MM(128)=.TRUE.
  REAL         :: R(128,128)= 1.1



  DO I1 = 1, 127
    IF (ANY(MAXLOC(ARRAY=R4(I1:127), MASK=MM(i1:127), KIND=1_1 ) .NE. 128-I1)) STOP 11
    IF (ANY(MAXLOC(ARRAY=R4(I1:127), MASK=MM(i1:127), KIND=1_2 ) .NE. 128-I1)) STOP 12
    IF (ANY(MAXLOC(ARRAY=R4(I1:127), MASK=MM(i1:127), KIND=1_4 ) .NE. 128-I1)) STOP 13
    IF (ANY(MAXLOC(ARRAY=R4(I1:127), MASK=MM(i1:127), KIND=1_8 ) .NE. 128-I1)) STOP 14
  END DO

  DO I2 = 1, 128
    IF (ANY(MAXLOC(ARRAY=R8(:I2), MASK=.TRUE._8, KIND=2_1 ) .NE. I2)) STOP 21
    IF (ANY(MAXLOC(ARRAY=R8(:I2), MASK=.TRUE._4, KIND=2_2 ) .NE. I2)) STOP 22
    IF (ANY(MAXLOC(ARRAY=R8(:I2), MASK=.TRUE._2, KIND=2_4 ) .NE. I2)) STOP 23
    IF (ANY(MAXLOC(ARRAY=R8(:I2), MASK=.TRUE._1, KIND=2_8 ) .NE. I2)) STOP 24
  END DO

  DO I4 =1, 128
    IF (ANY(MAXLOC(ARRAY=R6(1:I4), MASK= R4(1:I4) == R4(1:I4), KIND=4_1 ) .NE. I4)) STOP 41
    IF (ANY(MAXLOC(ARRAY=R6(1:I4), MASK= R4(1:I4) == R4(1:I4), KIND=4_2 ) .NE. I4)) STOP 42
    IF (ANY(MAXLOC(ARRAY=R6(1:I4), MASK= R4(1:I4) == R4(1:I4), KIND=4_4 ) .NE. I4)) STOP 43
    IF (ANY(MAXLOC(ARRAY=R6(1:I4), MASK= R4(1:I4) == R4(1:I4), KIND=4_8 ) .NE. I4)) STOP 44
  END DO

  IF (ANY(MAXLOC(ARRAY=R4(:), MASK=.FALSE._1, KIND=8_1 ) .NE. 0)) STOP 81
  IF (ANY(MAXLOC(ARRAY=R4(:), MASK=.FALSE._2, KIND=8_2 ) .NE. 0)) STOP 82
  IF (ANY(MAXLOC(ARRAY=R4(:), MASK=.FALSE._4, KIND=8_4 ) .NE. 0)) STOP 83
  IF (ANY(MAXLOC(ARRAY=R4(:), MASK=.FALSE._8, KIND=8_8 ) .NE. 0)) STOP 84

  IF (ANY(MAXLOC(ARRAY=R, DIM=1, KIND=2_1 ) .NE. 1)) STOP 91
  IF (ANY(MAXLOC(ARRAY=R, DIM=2, KIND=4_4 ) .NE. 1))  STOP 92


  END

