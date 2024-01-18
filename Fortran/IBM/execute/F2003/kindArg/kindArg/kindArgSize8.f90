!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgSize8
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
!*  -- assume sized array
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgSize8
  IMPLICIT NONE

  INTEGER I

  CLASS(*), ALLOCATABLE :: M(:,:,:)


  ALLOCATE(M(127,129,1) ,SOURCE=.FALSE._8)

  CALL IntSub(M)

  CONTAINS

  SUBROUTINE IntSub(M)
  CLASS(*) :: M(-126:0,129,*)

  DO I = 1, 129
    IF (     SIZE(M(:,I,1),   KIND=1 )            .NE. 127)          STOP 10
    IF (KIND(SIZE(M(:,I,1),   KIND=1 ))           .NE. 1)            STOP 11
!   IF (     SIZE(M(:,I:,1),  KIND=1 )            .NE. 127*(130-I))  STOP 12
    IF (KIND(SIZE(M(:,I:,1),  KIND=1 ))           .NE. 1)            STOP 13
!   IF (     SIZE(M(:,:,1),   KIND=1 )            .NE. 127*129)      STOP 14
    IF (KIND(SIZE(M(:,:,1),   KIND=1 ))           .NE. 1)            STOP 15
    IF (     SIZE(M,          KIND=1, DIM=1 )     .NE. 127)          STOP 16
    IF (KIND(SIZE(M,          KIND=1, DIM=1 ))    .NE. 1)            STOP 17
    IF (     SIZE(M,          KIND=1, DIM=2 )     .NE. 129)          STOP 18
    IF (KIND(SIZE(M,          KIND=1, DIM=2 ))    .NE. 1)            STOP 19
  END DO


  DO I = 1, 129
    IF (     SIZE(M(:,I,1),   KIND=2 )            .NE. 127)          STOP 20
    IF (KIND(SIZE(M(:,I,1),   KIND=2 ))           .NE. 2)            STOP 21
    IF (     SIZE(M(:,I:,1),  KIND=2 )            .NE. 127*(130-I))  STOP 22
    IF (KIND(SIZE(M(:,I:,1),  KIND=2 ))           .NE. 2)            STOP 23
    IF (     SIZE(M(:,:,1),   KIND=2 )            .NE. 127*129)      STOP 24
    IF (KIND(SIZE(M(:,:,1),   KIND=2 ))           .NE. 2)            STOP 25
    IF (     SIZE(M,          KIND=2, DIM=1 )     .NE. 127)          STOP 26
    IF (KIND(SIZE(M,          KIND=2, DIM=1 ))    .NE. 2)            STOP 27
    IF (     SIZE(M,          KIND=2, DIM=2 )     .NE. 129)          STOP 28
    IF (KIND(SIZE(M,          KIND=2, DIM=2 ))    .NE. 2)            STOP 29
  END DO


  DO I = 1, 129
    IF (     SIZE(M(:,I,1),   KIND=4 )            .NE. 127)          STOP 40
    IF (KIND(SIZE(M(:,I,1),   KIND=4 ))           .NE. 4)            STOP 41
    IF (     SIZE(M(:,I:,1),  KIND=4 )            .NE. 127*(130-I))  STOP 42
    IF (KIND(SIZE(M(:,I:,1),  KIND=4 ))           .NE. 4)            STOP 43
    IF (     SIZE(M(:,:,1),   KIND=4 )            .NE. 127*129)      STOP 44
    IF (KIND(SIZE(M(:,:,1),   KIND=4 ))           .NE. 4)            STOP 45
    IF (     SIZE(M,          KIND=4, DIM=1 )     .NE. 127)          STOP 46
    IF (KIND(SIZE(M,          KIND=4, DIM=1 ))    .NE. 4)            STOP 47
    IF (     SIZE(M,          KIND=4, DIM=2 )     .NE. 129)          STOP 48
    IF (KIND(SIZE(M,          KIND=4, DIM=2 ))    .NE. 4)            STOP 49
  END DO


  DO I = 1, 129
    IF (     SIZE(M(:,I,1),   KIND=8 )            .NE. 127)          STOP 80
    IF (KIND(SIZE(M(:,I,1),   KIND=8 ))           .NE. 8)            STOP 81
    IF (     SIZE(M(:,I:,1),  KIND=8 )            .NE. 127*(130-I))  STOP 82
    IF (KIND(SIZE(M(:,I:,1),  KIND=8 ))           .NE. 8)            STOP 83
    IF (     SIZE(M(:,:,1),   KIND=8 )            .NE. 127*129)      STOP 84
    IF (KIND(SIZE(M(:,:,1),   KIND=8 ))           .NE. 8)            STOP 85
    IF (     SIZE(M,          KIND=8, DIM=1 )     .NE. 127)          STOP 86
    IF (KIND(SIZE(M,          KIND=8, DIM=1 ))    .NE. 8)            STOP 87
    IF (     SIZE(M,          KIND=8, DIM=2 )     .NE. 129)          STOP 88
    IF (KIND(SIZE(M,          KIND=8, DIM=2 ))    .NE. 8)            STOP 89
  END DO



  END SUBROUTINE

  END



