!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 29, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SHAPE
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGEK(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Result Value. The value of the result is the shape of SOURCE.
!*
!*  - scalar
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgShape7
  IMPLICIT NONE


  IF (ANY( SHAPE(SOURCE=1_1, KIND=1_1 )  .NE. 0 ))  STOP 11
  IF (KIND(SHAPE(SOURCE=1_1, KIND=1_1 )) .NE. 1 )   STOP 12
  IF (ANY( SHAPE(SOURCE=2_2, KIND=2_2 )  .NE. 0 ))  STOP 13
  IF (KIND(SHAPE(SOURCE=2_2, KIND=2_2 )) .NE. 2 )   STOP 14
  IF (ANY( SHAPE(SOURCE=4_4, KIND=4_4 )  .NE. 0 ))  STOP 15
  IF (KIND(SHAPE(SOURCE=4_4, KIND=4_4 )) .NE. 4 )   STOP 16
  IF (ANY( SHAPE(SOURCE=8_8, KIND=8_8 )  .NE. 0 ))  STOP 17
  IF (KIND(SHAPE(SOURCE=8_8, KIND=8_8 )) .NE. 8 )   STOP 18


  IF (ANY( SHAPE(SOURCE=.TRUE._1,  KIND=1_1 )  .NE. 0 ))  STOP 21
  IF (KIND(SHAPE(SOURCE=.TRUE._1,  KIND=1_1 )) .NE. 1 )   STOP 22
  IF (ANY( SHAPE(SOURCE=.TRUE._2,  KIND=2_2 )  .NE. 0 ))  STOP 23
  IF (KIND(SHAPE(SOURCE=.TRUE._2,  KIND=2_2 )) .NE. 2 )   STOP 24
  IF (ANY( SHAPE(SOURCE=.FALSE._4, KIND=4_4 )  .NE. 0 ))  STOP 25
  IF (KIND(SHAPE(SOURCE=.FALSE._4, KIND=4_4 )) .NE. 4 )   STOP 26
  IF (ANY( SHAPE(SOURCE=.FALSE._8, KIND=8_8 )  .NE. 0 ))  STOP 27
  IF (KIND(SHAPE(SOURCE=.FALSE._8, KIND=8_8 )) .NE. 8 )   STOP 28


  IF (ANY( SHAPE(SOURCE=1._4,  KIND=4_4 )  .NE. 0 ))  STOP 31
  IF (KIND(SHAPE(SOURCE=1._4,  KIND=4_4 )) .NE. 4 )   STOP 32
  IF (ANY( SHAPE(SOURCE=8._8,  KIND=8_2 )  .NE. 0 ))  STOP 33
  IF (KIND(SHAPE(SOURCE=8._8,  KIND=8_2 )) .NE. 8 )   STOP 34
  IF (ANY( SHAPE(SOURCE=6._16, KIND=1_1 )  .NE. 0 ))  STOP 35
  IF (KIND(SHAPE(SOURCE=6._16, KIND=1_1 )) .NE. 1  )  STOP 36

  IF (ANY( SHAPE(SOURCE=(4._4,4._4),   KIND=4_4 )  .NE. 0 ))  STOP 41
  IF (KIND(SHAPE(SOURCE=(4._4,4._4),   KIND=4_4 )) .NE. 4 )   STOP 42
  IF (ANY( SHAPE(SOURCE=(8._8,8._8),   KIND=8_2 )  .NE. 0 ))  STOP 43
  IF (KIND(SHAPE(SOURCE=(8._8,8._8),   KIND=8_2 )) .NE. 8 )   STOP 44
  IF (ANY( SHAPE(SOURCE=(6._16,6._16), KIND=2_1 )  .NE. 0 ))  STOP 45
  IF (KIND(SHAPE(SOURCE=(6._16,6._16), KIND=2_1 )) .NE. 2  )  STOP 46


  END

