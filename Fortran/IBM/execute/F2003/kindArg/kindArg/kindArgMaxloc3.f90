!*********************************************************************
!*  ===================================================================
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
!*
!*  (325537)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMaxloc3

  !CHARACTER(*), PARAMETER  :: Str(0:127) = (/CHARACTER(3) :: ( ACHAR(I)//"  ", I=0,127) /)
  CHARACTER(*), PARAMETER  :: Str(0:127) = (/ ( ACHAR(I)//"  ", I=0,127) /)
  !CHARACTER(*), PARAMETER  :: Str(128) = (/( ACHAR(I)//"  ", I=0,127) /)
  CHARACTER(3)             :: Str1(128,128)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  LOGICAL      :: MM(128)=.TRUE.


  DO I=1, 128
   Str1(I,:) = (/( ACHAR(I)//"  ", I=0,127) /)
  END DO

  IF (ANY( MAXLOC(ARRAY=Str(0:126), MASK=MM(1:127), KIND=1_1 ) .NE. 127)) ERROR STOP 11
  IF (ANY( MAXLOC(ARRAY=Str(0:126), MASK=MM(1:127), KIND=1_2 ) .NE. 127)) ERROR STOP 12
  IF (ANY( MAXLOC(ARRAY=Str(0:126), MASK=MM(1:127), KIND=1_4 ) .NE. 127)) ERROR STOP 13
  IF (ANY( MAXLOC(ARRAY=Str(0:126), MASK=MM(1:127), KIND=1_8 ) .NE. 127)) ERROR STOP 14

  IF (ANY( MAXLOC(ARRAY=Str, MASK=.TRUE._8, KIND=2_1 ) .NE. 128)) ERROR STOP 21
  IF (ANY( MAXLOC(ARRAY=Str, MASK=.TRUE._4, KIND=2_2 ) .NE. 128)) ERROR STOP 22
  IF (ANY( MAXLOC(ARRAY=Str, MASK=.TRUE._2, KIND=2_4 ) .NE. 128)) ERROR STOP 23
  IF (ANY( MAXLOC(ARRAY=Str, MASK=.TRUE._1, KIND=2_8 ) .NE. 128)) ERROR STOP 24

  IF (ANY( MAXLOC(ARRAY=Str, MASK= Str == Str, KIND=4_1 ) .NE. 128)) ERROR STOP 41
  IF (ANY( MAXLOC(ARRAY=Str, MASK= Str == Str, KIND=4_2 ) .NE. 128)) ERROR STOP 42
  IF (ANY( MAXLOC(ARRAY=Str, MASK= Str == Str, KIND=4_4 ) .NE. 128)) ERROR STOP 43
  IF (ANY( MAXLOC(ARRAY=Str, MASK= Str == Str, KIND=4_8 ) .NE. 128)) ERROR STOP 44

  IF (ANY( MAXLOC(ARRAY=Str(:), MASK=.FALSE._1, KIND=8_1 ) .NE. 0)) ERROR STOP 81
  IF (ANY( MAXLOC(ARRAY=Str(:), MASK=.FALSE._2, KIND=8_2 ) .NE. 0)) ERROR STOP 82
  IF (ANY( MAXLOC(ARRAY=Str(:), MASK=.FALSE._4, KIND=8_4 ) .NE. 0)) ERROR STOP 83
  IF (ANY( MAXLOC(ARRAY=Str(:), MASK=.FALSE._8, KIND=8_8 ) .NE. 0)) ERROR STOP 84

  IF (ANY(MAXLOC(ARRAY=Str1, DIM=1, KIND=2_1 ) .NE. 1 ))  ERROR STOP 91
  IF (ANY(MAXLOC(ARRAY=Str1, DIM=2, KIND=4_4 ) .NE. 128))  ERROR STOP 92


  END

