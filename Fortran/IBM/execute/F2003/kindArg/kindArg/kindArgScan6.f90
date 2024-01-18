!*********************************************************************
!*  ===================================================================
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
!*  Result Value.
!*  Case (ii): If BACK is present with the value true and if STRING contains at least one
!*  character that is in SET, the value of the result is the position of the rightmost
!*  character of STRING that is in SET.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgScan6
  IMPLICIT NONE

  TYPE :: DT
    INTEGER(1),   ALLOCATABLE :: K1(:,:)
    INTEGER(2),   POINTER     :: K2(:,:)
    INTEGER(4),   ALLOCATABLE :: K4(:,:)
    INTEGER(8),   POINTER     :: K8(:,:)
    CHARACTER(:), ALLOCATABLE :: CC(:)
  END TYPE

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4, I
  INTEGER(8) :: I8


  CLASS(DT), POINTER :: T

  ALLOCATE(T)
  ALLOCATE(T%CC(127), SOURCE=(/(CHAR(I)//CHAR(I)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0), I=1, 127)/))

  ASSOCIATE ( TT => T)
  SELECT TYPE ( TT )
  TYPE IS (DT)

  DO I4 = 1, 127
    IF (     SCAN(STRING=T%CC(I4), SET=T%CC(I4)(1:2), BACK=.TRUE., KIND=KIND(T%K8))   .NE. 2)               ERROR STOP 41
    IF (KIND(SCAN(STRING=T%CC(I4), SET=T%CC(I4)(1:2), BACK=.TRUE., KIND=KIND(T%K8)))  .NE. 8)               ERROR STOP 42
    IF (     SCAN(STRING=T%CC(I4), SET=T%CC(I4)(1:2), BACK=.TRUE., KIND=KIND(T%K4))   .NE. 2)               ERROR STOP 43
    IF (KIND(SCAN(STRING=T%CC(I4), SET=T%CC(I4)(1:2), BACK=.TRUE., KIND=KIND(T%K4)))  .NE. 4)               ERROR STOP 44
    IF (     SCAN(STRING=T%CC(I4), SET=T%CC(I4)(1:2), BACK=.TRUE., KIND=KIND(T%K2))   .NE. 2)               ERROR STOP 45
    IF (KIND(SCAN(STRING=T%CC(I4), SET=T%CC(I4)(1:2), BACK=.TRUE., KIND=KIND(T%K2)))  .NE. 2)               ERROR STOP 46
    IF (     SCAN(STRING=T%CC(I4), SET=T%CC(I4)(1:2), BACK=.TRUE., KIND=KIND(T%K1))   .NE. 2)               ERROR STOP 47
    IF (KIND(SCAN(STRING=T%CC(I4), SET=T%CC(I4)(1:2), BACK=.TRUE., KIND=KIND(T%K1)))  .NE. 1)               ERROR STOP 48
  END DO

  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(1:2), BACK=.TRUE., KIND=KIND(T%K8))   .NE. 2))         ERROR STOP 61
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(1:2), BACK=.TRUE., KIND=KIND(T%K8)))  .NE. 8)          ERROR STOP 62
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(1:2), BACK=.TRUE., KIND=KIND(T%K4))   .NE. 2))         ERROR STOP 63
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(1:2), BACK=.TRUE., KIND=KIND(T%K4)))  .NE. 4)          ERROR STOP 64
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(1:2), BACK=.TRUE., KIND=KIND(T%K2))   .NE. 2))         ERROR STOP 65
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(1:2), BACK=.TRUE., KIND=KIND(T%K2)))  .NE. 2)          ERROR STOP 66
  IF (ANY( SCAN(STRING=T%CC, SET=T%CC(:)(1:2), BACK=.TRUE., KIND=KIND(T%K1))   .NE. 2))         ERROR STOP 67
  IF (KIND(SCAN(STRING=T%CC, SET=T%CC(:)(1:2), BACK=.TRUE., KIND=KIND(T%K1)))  .NE. 1)          ERROR STOP 68


  CLASS DEFAULT
    STOP 88
  END SELECT
  END ASSOCIATE


  END


