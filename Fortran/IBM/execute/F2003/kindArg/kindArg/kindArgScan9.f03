!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : INDEX
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
!*  -qintsize
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgScan9


  INTEGER(4) :: I4
  INTEGER(8) :: I8

  INTEGER   :: I, J
  CHARACTER(LEN=128*8) :: CC(128)


  DO I=1, LEN(CC)
    CC(:)(I:I)= ACHAR(MOD(I,128))
  END DO


  DO I4 = 1, 128
    IF (     SCAN(STRING=CC(I4), SET=CC(I4)(I4:I4))   .NE. I4)               ERROR STOP 11
    IF (KIND(SCAN(STRING=CC(I4), SET=CC(I4)(I4:I4)))  .NE. II%KIND)          ERROR STOP 12
  END DO

  DO I4 = 1, 128
    IF (     SCAN(STRING=CC(I4), SET=CC(I4)(I4:I4), BACK=.FALSE._4 )   .NE. I4)               ERROR STOP 21
    IF (KIND(SCAN(STRING=CC(I4), SET=CC(I4)(I4:I4), BACK=.FALSE._4 ))  .NE. II%KIND)          ERROR STOP 22
  END DO

  DO I8 = 1, 128
    IF (     SCAN(STRING=CC(I8), SET=CC(I8)(I8:I8), BACK=.TRUE._1)   .NE. LEN(CC)-(128-I8))   ERROR STOP 31
    IF (KIND(SCAN(STRING=CC(I8), SET=CC(I8)(I8:I8), BACK=.TRUE._1))  .NE. II%KIND)            ERROR STOP 32
  END DO

  IF (ANY( SCAN(STRING=CC, SET=CC)   .NE. 1))           ERROR STOP 41
  IF (KIND(SCAN(STRING=CC, SET=CC))  .NE. II%KIND)      ERROR STOP 42

  IF (ANY( SCAN(STRING=CC, SET=CC, BACK=.FALSE.)   .NE. 1))         ERROR STOP 51
  IF (KIND(SCAN(STRING=CC, SET=CC, BACK=.FALSE.))  .NE. II%KIND)    ERROR STOP 52

  IF (ANY( SCAN(STRING=CC, SET=CC, BACK=.TRUE.)   .NE. LEN(CC)))    ERROR STOP 61
  IF (KIND(SCAN(STRING=CC, SET=CC, BACK=.TRUE.))  .NE. II%KIND)     ERROR STOP 62

  IF (ANY( SCAN(STRING=CC(1), SET=CC)   .NE. 1))           ERROR STOP 71
  IF (KIND(SCAN(STRING=CC(1), SET=CC))  .NE. II%KIND)      ERROR STOP 72


  END

