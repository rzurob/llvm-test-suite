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
!*  Case (iii): The value of the result 1 is zero if no character of STRING is in SET or if the
!*  length of STRING or SET is zero.
!*
!*  ()
!*
!234767890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgScan7

  CHARACTER(:), ALLOCATABLE :: CC(:)
  LOGICAL(1),   ALLOCATABLE :: K1(:,:)
  LOGICAL(2),   POINTER     :: K2(:,:)
  LOGICAL(4),   ALLOCATABLE :: K4(:,:)
  LOGICAL(8),   POINTER     :: K8(:,:)

  CHARACTER(3)              :: SET=ACHAR(0)//ACHAR(0)//ACHAR(0)

  ALLOCATE(CC(127), SOURCE=(/(CHAR(I)//CHAR(I)//CHAR(I)//CHAR(I)//CHAR(I)//CHAR(I)//CHAR(I), I=1, 127)/))

  ASSOCIATE (CC => CC)

! Case 1

  DO I1 = 1, 127
    IF (     SCAN(STRING=CC(I1), SET=SET, KIND=KIND(K8))   .NE. 0)               STOP 11
    IF (KIND(SCAN(STRING=CC(I1), SET=SET, KIND=KIND(K8)))  .NE. 8)               STOP 12
    IF (     SCAN(STRING=CC(I1), SET=SET, KIND=KIND(K4))   .NE. 0)               STOP 13
    IF (KIND(SCAN(STRING=CC(I1), SET=SET, KIND=KIND(K4)))  .NE. 4)               STOP 14
    IF (     SCAN(STRING=CC(I1), SET=SET, KIND=KIND(K2))   .NE. 0)               STOP 17
    IF (KIND(SCAN(STRING=CC(I1), SET=SET, KIND=KIND(K2)))  .NE. 2)               STOP 16
    IF (     SCAN(STRING=CC(I1), SET=SET, KIND=KIND(K1))   .NE. 0)               STOP 17
    IF (KIND(SCAN(STRING=CC(I1), SET=SET, KIND=KIND(K1)))  .NE. 1)               STOP 18
  END DO


  DO I4 = 1, 127
    IF (     SCAN(STRING=CC(I4), SET=SET, BACK=.FALSE., KIND=KIND(K8))   .NE. 0)               STOP 21
    IF (KIND(SCAN(STRING=CC(I4), SET=SET, BACK=.FALSE., KIND=KIND(K8)))  .NE. 8)               STOP 22
    IF (     SCAN(STRING=CC(I4), SET=SET, BACK=.FALSE., KIND=KIND(K4))   .NE. 0)               STOP 23
    IF (KIND(SCAN(STRING=CC(I4), SET=SET, BACK=.FALSE., KIND=KIND(K4)))  .NE. 4)               STOP 24
    IF (     SCAN(STRING=CC(I4), SET=SET, BACK=.FALSE., KIND=KIND(K2))   .NE. 0)               STOP 27
    IF (KIND(SCAN(STRING=CC(I4), SET=SET, BACK=.FALSE., KIND=KIND(K2)))  .NE. 2)               STOP 26
    IF (     SCAN(STRING=CC(I4), SET=SET, BACK=.FALSE., KIND=KIND(K1))   .NE. 0)               STOP 27
    IF (KIND(SCAN(STRING=CC(I4), SET=SET, BACK=.FALSE., KIND=KIND(K1)))  .NE. 1)               STOP 28
  END DO


  IF (ANY( SCAN(STRING=CC, SET=SET, KIND=KIND(K8))   .NE. 0))           STOP 31
  IF (KIND(SCAN(STRING=CC, SET=SET, KIND=KIND(K8)))  .NE. 8)            STOP 32
  IF (ANY( SCAN(STRING=CC, SET=SET, KIND=KIND(K4))   .NE. 0))           STOP 33
  IF (KIND(SCAN(STRING=CC, SET=SET, KIND=KIND(K4)))  .NE. 4)            STOP 34
  IF (ANY( SCAN(STRING=CC, SET=SET, KIND=KIND(K2))   .NE. 0))           STOP 35
  IF (KIND(SCAN(STRING=CC, SET=SET, KIND=KIND(K2)))  .NE. 2)            STOP 36
  IF (ANY( SCAN(STRING=CC, SET=SET, KIND=KIND(K1))   .NE. 0))           STOP 37
  IF (KIND(SCAN(STRING=CC, SET=SET, KIND=KIND(K1)))  .NE. 1)            STOP 38

  IF (ANY( SCAN(STRING=CC, SET=SET, BACK=.FALSE., KIND=KIND(K8))   .NE. 0))         STOP 41
  IF (KIND(SCAN(STRING=CC, SET=SET, BACK=.FALSE., KIND=KIND(K8)))  .NE. 8)          STOP 42
  IF (ANY( SCAN(STRING=CC, SET=SET, BACK=.FALSE., KIND=KIND(K4))   .NE. 0))         STOP 43
  IF (KIND(SCAN(STRING=CC, SET=SET, BACK=.FALSE., KIND=KIND(K4)))  .NE. 4)          STOP 44
  IF (ANY( SCAN(STRING=CC, SET=SET, BACK=.FALSE., KIND=KIND(K2))   .NE. 0))         STOP 47
  IF (KIND(SCAN(STRING=CC, SET=SET, BACK=.FALSE., KIND=KIND(K2)))  .NE. 2)          STOP 46
  IF (ANY( SCAN(STRING=CC, SET=SET, BACK=.FALSE., KIND=KIND(K1))   .NE. 0))         STOP 47
  IF (KIND(SCAN(STRING=CC, SET=SET, BACK=.FALSE., KIND=KIND(K1)))  .NE. 1)          STOP 48

! case 2
  DO I1 = 1, 127
    IF (     SCAN(STRING=CC(I1), SET=SET(1:0), KIND=KIND(K8))   .NE. 0)               STOP 51
    IF (KIND(SCAN(STRING=CC(I1), SET=SET(1:0), KIND=KIND(K8)))  .NE. 8)               STOP 52
    IF (     SCAN(STRING=CC(I1), SET=SET(1:0), KIND=KIND(K4))   .NE. 0)               STOP 53
    IF (KIND(SCAN(STRING=CC(I1), SET=SET(1:0), KIND=KIND(K4)))  .NE. 4)               STOP 54
    IF (     SCAN(STRING=CC(I1), SET=SET(1:0), KIND=KIND(K2))   .NE. 0)               STOP 57
    IF (KIND(SCAN(STRING=CC(I1), SET=SET(1:0), KIND=KIND(K2)))  .NE. 2)               STOP 56
    IF (     SCAN(STRING=CC(I1), SET=SET(1:0), KIND=KIND(K1))   .NE. 0)               STOP 57
    IF (KIND(SCAN(STRING=CC(I1), SET=SET(1:0), KIND=KIND(K1)))  .NE. 1)               STOP 58
  END DO


  DO I4 = 1, 127
    IF (     SCAN(STRING=CC(I4), SET=SET(1:0), BACK=.FALSE., KIND=KIND(K8))   .NE. 0)               STOP 61
    IF (KIND(SCAN(STRING=CC(I4), SET=SET(1:0), BACK=.FALSE., KIND=KIND(K8)))  .NE. 8)               STOP 62
    IF (     SCAN(STRING=CC(I4), SET=SET(1:0), BACK=.FALSE., KIND=KIND(K4))   .NE. 0)               STOP 63
    IF (KIND(SCAN(STRING=CC(I4), SET=SET(1:0), BACK=.FALSE., KIND=KIND(K4)))  .NE. 4)               STOP 64
    IF (     SCAN(STRING=CC(I4), SET=SET(1:0), BACK=.FALSE., KIND=KIND(K2))   .NE. 0)               STOP 67
    IF (KIND(SCAN(STRING=CC(I4), SET=SET(1:0), BACK=.FALSE., KIND=KIND(K2)))  .NE. 2)               STOP 66
    IF (     SCAN(STRING=CC(I4), SET=SET(1:0), BACK=.FALSE., KIND=KIND(K1))   .NE. 0)               STOP 67
    IF (KIND(SCAN(STRING=CC(I4), SET=SET(1:0), BACK=.FALSE., KIND=KIND(K1)))  .NE. 1)               STOP 68
  END DO


  IF (ANY( SCAN(STRING=CC, SET=SET(1:0), KIND=KIND(K8))   .NE. 0))           STOP 71
  IF (KIND(SCAN(STRING=CC, SET=SET(1:0), KIND=KIND(K8)))  .NE. 8)            STOP 72
  IF (ANY( SCAN(STRING=CC, SET=SET(1:0), KIND=KIND(K4))   .NE. 0))           STOP 73
  IF (KIND(SCAN(STRING=CC, SET=SET(1:0), KIND=KIND(K4)))  .NE. 4)            STOP 74
  IF (ANY( SCAN(STRING=CC, SET=SET(1:0), KIND=KIND(K2))   .NE. 0))           STOP 75
  IF (KIND(SCAN(STRING=CC, SET=SET(1:0), KIND=KIND(K2)))  .NE. 2)            STOP 76
  IF (ANY( SCAN(STRING=CC, SET=SET(1:0), KIND=KIND(K1))   .NE. 0))           STOP 77
  IF (KIND(SCAN(STRING=CC, SET=SET(1:0), KIND=KIND(K1)))  .NE. 1)            STOP 78

  IF (ANY( SCAN(STRING=CC, SET=SET(1:0), BACK=.FALSE., KIND=KIND(K8))   .NE. 0))         STOP 81
  IF (KIND(SCAN(STRING=CC, SET=SET(1:0), BACK=.FALSE., KIND=KIND(K8)))  .NE. 8)          STOP 82
  IF (ANY( SCAN(STRING=CC, SET=SET(1:0), BACK=.FALSE., KIND=KIND(K4))   .NE. 0))         STOP 83
  IF (KIND(SCAN(STRING=CC, SET=SET(1:0), BACK=.FALSE., KIND=KIND(K4)))  .NE. 4)          STOP 84
  IF (ANY( SCAN(STRING=CC, SET=SET(1:0), BACK=.FALSE., KIND=KIND(K2))   .NE. 0))         STOP 87
  IF (KIND(SCAN(STRING=CC, SET=SET(1:0), BACK=.FALSE., KIND=KIND(K2)))  .NE. 2)          STOP 86
  IF (ANY( SCAN(STRING=CC, SET=SET(1:0), BACK=.FALSE., KIND=KIND(K1))   .NE. 0))         STOP 87
  IF (KIND(SCAN(STRING=CC, SET=SET(1:0), BACK=.FALSE., KIND=KIND(K1)))  .NE. 1)          STOP 88

! Case 3

  DO I1 = 1, 127
    IF (     SCAN(STRING=CC(I1)(1:0), SET=SET, KIND=KIND(K8))   .NE. 0)               STOP 111
    IF (KIND(SCAN(STRING=CC(I1)(1:0), SET=SET, KIND=KIND(K8)))  .NE. 8)               STOP 112
    IF (     SCAN(STRING=CC(I1)(1:0), SET=SET, KIND=KIND(K4))   .NE. 0)               STOP 113
    IF (KIND(SCAN(STRING=CC(I1)(1:0), SET=SET, KIND=KIND(K4)))  .NE. 4)               STOP 114
    IF (     SCAN(STRING=CC(I1)(1:0), SET=SET, KIND=KIND(K2))   .NE. 0)               STOP 117
    IF (KIND(SCAN(STRING=CC(I1)(1:0), SET=SET, KIND=KIND(K2)))  .NE. 2)               STOP 116
    IF (     SCAN(STRING=CC(I1)(1:0), SET=SET, KIND=KIND(K1))   .NE. 0)               STOP 117
    IF (KIND(SCAN(STRING=CC(I1)(1:0), SET=SET, KIND=KIND(K1)))  .NE. 1)               STOP 118
  END DO


  DO I4 = 1, 127
    IF (     SCAN(STRING=CC(I4)(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K8))   .NE. 0)               STOP 121
    IF (KIND(SCAN(STRING=CC(I4)(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K8)))  .NE. 8)               STOP 122
    IF (     SCAN(STRING=CC(I4)(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K4))   .NE. 0)               STOP 123
    IF (KIND(SCAN(STRING=CC(I4)(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K4)))  .NE. 4)               STOP 124
    IF (     SCAN(STRING=CC(I4)(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K2))   .NE. 0)               STOP 127
    IF (KIND(SCAN(STRING=CC(I4)(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K2)))  .NE. 2)               STOP 126
    IF (     SCAN(STRING=CC(I4)(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K1))   .NE. 0)               STOP 127
    IF (KIND(SCAN(STRING=CC(I4)(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K1)))  .NE. 1)               STOP 128
  END DO


  IF (ANY( SCAN(STRING=CC(1:0), SET=SET, KIND=KIND(K8))   .NE. 0))           STOP 131
  IF (KIND(SCAN(STRING=CC(1:0), SET=SET, KIND=KIND(K8)))  .NE. 8)            STOP 132
  IF (ANY( SCAN(STRING=CC(1:0), SET=SET, KIND=KIND(K4))   .NE. 0))           STOP 133
  IF (KIND(SCAN(STRING=CC(1:0), SET=SET, KIND=KIND(K4)))  .NE. 4)            STOP 134
  IF (ANY( SCAN(STRING=CC(1:0), SET=SET, KIND=KIND(K2))   .NE. 0))           STOP 135
  IF (KIND(SCAN(STRING=CC(1:0), SET=SET, KIND=KIND(K2)))  .NE. 2)            STOP 136
  IF (ANY( SCAN(STRING=CC(1:0), SET=SET, KIND=KIND(K1))   .NE. 0))           STOP 137
  IF (KIND(SCAN(STRING=CC(1:0), SET=SET, KIND=KIND(K1)))  .NE. 1)            STOP 138

  IF (ANY( SCAN(STRING=CC(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K8))   .NE. 0))         STOP 141
  IF (KIND(SCAN(STRING=CC(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K8)))  .NE. 8)          STOP 142
  IF (ANY( SCAN(STRING=CC(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K4))   .NE. 0))         STOP 143
  IF (KIND(SCAN(STRING=CC(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K4)))  .NE. 4)          STOP 144
  IF (ANY( SCAN(STRING=CC(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K2))   .NE. 0))         STOP 147
  IF (KIND(SCAN(STRING=CC(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K2)))  .NE. 2)          STOP 146
  IF (ANY( SCAN(STRING=CC(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K1))   .NE. 0))         STOP 147
  IF (KIND(SCAN(STRING=CC(1:0), SET=SET, BACK=.FALSE., KIND=KIND(K1)))  .NE. 1)          STOP 148


  END ASSOCIATE

  END

