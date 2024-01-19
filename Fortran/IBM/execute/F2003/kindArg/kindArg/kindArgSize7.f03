!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SIZE
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
!*  Entities of different types for ARRAY
!*  zero sized array
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgSize7
  IMPLICIT NONE

  INTEGER I

  CLASS(*), ALLOCATABLE :: M(:,:)


  ALLOCATE(M(1:0,127) ,SOURCE=.FALSE._8)


  DO I = 1, 127
    IF (     SIZE(M(:,I),   KIND=1 )            .NE. 0)          ERROR STOP 10
    IF (KIND(SIZE(M(:,I),   KIND=1 ))           .NE. 1)          ERROR STOP 11
    IF (     SIZE(M(:,I:),  KIND=1 )            .NE. 0)          ERROR STOP 12
    IF (KIND(SIZE(M(:,I:),  KIND=1 ))           .NE. 1)          ERROR STOP 13
    IF (     SIZE(M,        KIND=1 )            .NE. 0)          ERROR STOP 14
    IF (KIND(SIZE(M,        KIND=1 ))           .NE. 1)          ERROR STOP 15
    IF (     SIZE(M,        KIND=1, DIM=1 )     .NE. 0)          ERROR STOP 16
    IF (KIND(SIZE(M,        KIND=1, DIM=1 ))    .NE. 1)          ERROR STOP 17
    IF (     SIZE(M,        KIND=1, DIM=2 )     .NE. 127)        ERROR STOP 18
    IF (KIND(SIZE(M,        KIND=1, DIM=2 ))    .NE. 1)          ERROR STOP 19
  END DO


  DO I = 1, 127
    IF (     SIZE(M(:,I),   KIND=2 )            .NE. 0)          ERROR STOP 20
    IF (KIND(SIZE(M(:,I),   KIND=2 ))           .NE. 2)          ERROR STOP 21
    IF (     SIZE(M(:,I:),  KIND=2 )            .NE. 0)          ERROR STOP 22
    IF (KIND(SIZE(M(:,I:),  KIND=2 ))           .NE. 2)          ERROR STOP 23
    IF (     SIZE(M,        KIND=2 )            .NE. 0)          ERROR STOP 24
    IF (KIND(SIZE(M,        KIND=2 ))           .NE. 2)          ERROR STOP 25
    IF (     SIZE(M,        KIND=2, DIM=1 )     .NE. 0)          ERROR STOP 26
    IF (KIND(SIZE(M,        KIND=2, DIM=1 ))    .NE. 2)          ERROR STOP 27
    IF (     SIZE(M,        KIND=2, DIM=2 )     .NE. 127)        ERROR STOP 28
    IF (KIND(SIZE(M,        KIND=2, DIM=2 ))    .NE. 2)          ERROR STOP 29
  END DO


  DO I = 1, 127
    IF (     SIZE(M(:,I),   KIND=4 )            .NE. 0)          ERROR STOP 40
    IF (KIND(SIZE(M(:,I),   KIND=4 ))           .NE. 4)          ERROR STOP 41
    IF (     SIZE(M(:,I:),  KIND=4 )            .NE. 0)          ERROR STOP 42
    IF (KIND(SIZE(M(:,I:),  KIND=4 ))           .NE. 4)          ERROR STOP 43
    IF (     SIZE(M,        KIND=4 )            .NE. 0)          ERROR STOP 44
    IF (KIND(SIZE(M,        KIND=4 ))           .NE. 4)          ERROR STOP 45
    IF (     SIZE(M,        KIND=4, DIM=1 )     .NE. 0)          ERROR STOP 46
    IF (KIND(SIZE(M,        KIND=4, DIM=1 ))    .NE. 4)          ERROR STOP 47
    IF (     SIZE(M,        KIND=4, DIM=2 )     .NE. 127)        ERROR STOP 48
    IF (KIND(SIZE(M,        KIND=4, DIM=2 ))    .NE. 4)          ERROR STOP 49
  END DO


  DO I = 1, 127
    IF (     SIZE(M(:,I),   KIND=8 )            .NE. 0)          ERROR STOP 80
    IF (KIND(SIZE(M(:,I),   KIND=8 ))           .NE. 8)          ERROR STOP 81
    IF (     SIZE(M(:,I:),  KIND=8 )            .NE. 0)          ERROR STOP 82
    IF (KIND(SIZE(M(:,I:),  KIND=8 ))           .NE. 8)          ERROR STOP 83
    IF (     SIZE(M,        KIND=8 )            .NE. 0)          ERROR STOP 84
    IF (KIND(SIZE(M,        KIND=8 ))           .NE. 8)          ERROR STOP 85
    IF (     SIZE(M,        KIND=8, DIM=1 )     .NE. 0)          ERROR STOP 86
    IF (KIND(SIZE(M,        KIND=8, DIM=1 ))    .NE. 8)          ERROR STOP 87
    IF (     SIZE(M,        KIND=8, DIM=2 )     .NE. 127)        ERROR STOP 88
    IF (KIND(SIZE(M,        KIND=8, DIM=2 ))    .NE. 8)          ERROR STOP 89
  END DO


  END



