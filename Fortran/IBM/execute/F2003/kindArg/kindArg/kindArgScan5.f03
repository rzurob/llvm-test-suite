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
!*  Result Value.
!*  Case (i): If BACK is absent or is present with the value false and if STRING contains at
!*  least one character that is in SET, the value of the result is the position of the leftmost
!*  character of STRING that is in SET.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgScan5

  CLASS(*), POINTER :: T(:)
  INTEGER(1), ALLOCATABLE :: K1(:,:)
  INTEGER(2), POINTER     :: K2(:,:)
  INTEGER(4), ALLOCATABLE :: K4(:,:)
  INTEGER(8), POINTER     :: K8(:,:)



  ALLOCATE(T(127), SOURCE=(/(CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(I)//CHAR(I), I=1, 127)/))

  ASSOCIATE ( TT => T)
  SELECT TYPE (CC => TT)
  TYPE IS (CHARACTER(*))
  DO I1 = 1, 127
    IF (     SCAN(STRING=CC(I1), SET=CC(I1)(7:7), KIND=KIND(K8))   .NE. 6)               ERROR STOP 11
    IF (KIND(SCAN(STRING=CC(I1), SET=CC(I1)(7:7), KIND=KIND(K8)))  .NE. 8)               ERROR STOP 12
    IF (     SCAN(STRING=CC(I1), SET=CC(I1)(7:7), KIND=KIND(K4))   .NE. 6)               ERROR STOP 13
    IF (KIND(SCAN(STRING=CC(I1), SET=CC(I1)(7:7), KIND=KIND(K4)))  .NE. 4)               ERROR STOP 14
    IF (     SCAN(STRING=CC(I1), SET=CC(I1)(7:7), KIND=KIND(K2))   .NE. 6)               ERROR STOP 15
    IF (KIND(SCAN(STRING=CC(I1), SET=CC(I1)(7:7), KIND=KIND(K2)))  .NE. 2)               ERROR STOP 16
    IF (     SCAN(STRING=CC(I1), SET=CC(I1)(7:7), KIND=KIND(K1))   .NE. 6)               ERROR STOP 17
    IF (KIND(SCAN(STRING=CC(I1), SET=CC(I1)(7:7), KIND=KIND(K1)))  .NE. 1)               ERROR STOP 18
  END DO


  DO I4 = 1, 127
    IF (     SCAN(STRING=CC(I4), SET=CC(I4)(7:7), BACK=.FALSE., KIND=KIND(K8))   .NE. 6)               ERROR STOP 41
    IF (KIND(SCAN(STRING=CC(I4), SET=CC(I4)(7:7), BACK=.FALSE., KIND=KIND(K8)))  .NE. 8)               ERROR STOP 42
    IF (     SCAN(STRING=CC(I4), SET=CC(I4)(7:7), BACK=.FALSE., KIND=KIND(K4))   .NE. 6)               ERROR STOP 43
    IF (KIND(SCAN(STRING=CC(I4), SET=CC(I4)(7:7), BACK=.FALSE., KIND=KIND(K4)))  .NE. 4)               ERROR STOP 44
    IF (     SCAN(STRING=CC(I4), SET=CC(I4)(7:7), BACK=.FALSE., KIND=KIND(K2))   .NE. 6)               ERROR STOP 45
    IF (KIND(SCAN(STRING=CC(I4), SET=CC(I4)(7:7), BACK=.FALSE., KIND=KIND(K2)))  .NE. 2)               ERROR STOP 46
    IF (     SCAN(STRING=CC(I4), SET=CC(I4)(7:7), BACK=.FALSE., KIND=KIND(K1))   .NE. 6)               ERROR STOP 47
    IF (KIND(SCAN(STRING=CC(I4), SET=CC(I4)(7:7), BACK=.FALSE., KIND=KIND(K1)))  .NE. 1)               ERROR STOP 48
  END DO

  IF (ANY( SCAN(STRING=CC, SET=CC(:)(7:7), KIND=KIND(K8))   .NE. 6))           ERROR STOP 51
  IF (KIND(SCAN(STRING=CC, SET=CC(:)(7:7), KIND=KIND(K8)))  .NE. 8)            ERROR STOP 52
  IF (ANY( SCAN(STRING=CC, SET=CC(:)(7:7), KIND=KIND(K4))   .NE. 6))           ERROR STOP 53
  IF (KIND(SCAN(STRING=CC, SET=CC(:)(7:7), KIND=KIND(K4)))  .NE. 4)            ERROR STOP 54
  IF (ANY( SCAN(STRING=CC, SET=CC(:)(7:7), KIND=KIND(K2))   .NE. 6))           ERROR STOP 55
  IF (KIND(SCAN(STRING=CC, SET=CC(:)(7:7), KIND=KIND(K2)))  .NE. 2)            ERROR STOP 56
  IF (ANY( SCAN(STRING=CC, SET=CC(:)(7:7), KIND=KIND(K1))   .NE. 6))           ERROR STOP 57
  IF (KIND(SCAN(STRING=CC, SET=CC(:)(7:7), KIND=KIND(K1)))  .NE. 1)            ERROR STOP 58

  IF (ANY( SCAN(STRING=CC, SET=CC(:)(7:7), BACK=.FALSE., KIND=KIND(K8))   .NE. 6))         ERROR STOP 61
  IF (KIND(SCAN(STRING=CC, SET=CC(:)(7:7), BACK=.FALSE., KIND=KIND(K8)))  .NE. 8)          ERROR STOP 62
  IF (ANY( SCAN(STRING=CC, SET=CC(:)(7:7), BACK=.FALSE., KIND=KIND(K4))   .NE. 6))         ERROR STOP 63
  IF (KIND(SCAN(STRING=CC, SET=CC(:)(7:7), BACK=.FALSE., KIND=KIND(K4)))  .NE. 4)          ERROR STOP 64
  IF (ANY( SCAN(STRING=CC, SET=CC(:)(7:7), BACK=.FALSE., KIND=KIND(K2))   .NE. 6))         ERROR STOP 65
  IF (KIND(SCAN(STRING=CC, SET=CC(:)(7:7), BACK=.FALSE., KIND=KIND(K2)))  .NE. 2)          ERROR STOP 66
  IF (ANY( SCAN(STRING=CC, SET=CC(:)(7:7), BACK=.FALSE., KIND=KIND(K1))   .NE. 6))         ERROR STOP 67
  IF (KIND(SCAN(STRING=CC, SET=CC(:)(7:7), BACK=.FALSE., KIND=KIND(K1)))  .NE. 1)          ERROR STOP 68


  CLASS DEFAULT
    STOP 88
  END SELECT
  END ASSOCIATE


  END
