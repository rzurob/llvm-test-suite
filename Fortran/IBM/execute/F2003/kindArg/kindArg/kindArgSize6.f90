!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgSize6
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
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgSize6
  IMPLICIT NONE

  INTEGER I

  CLASS(*), ALLOCATABLE :: MM1(:,:)
  CLASS(*), POINTER     :: MM2(:,:)
  CLASS(*), ALLOCATABLE :: MM4(:,:)
  CLASS(*), POINTER     :: MM8(:,:)

  ALLOCATE(MM1(127,129) ,SOURCE=.FALSE._1)
  ALLOCATE(MM2(127,129) ,SOURCE=.FALSE._2)
  ALLOCATE(MM4(127,129) ,SOURCE=.FALSE._4)
  ALLOCATE(MM8(127,129) ,SOURCE=.FALSE._8)



  ASSOCIATE (T1=>MM1, T2=>MM2, T4=>MM4, T8=>MM8)
  SELECT TYPE(M1=>T1)
  TYPE IS (LOGICAL(1))
  SELECT TYPE(M2=>T2)
  TYPE IS (LOGICAL(2))
  SELECT TYPE(M4=>T4)
  TYPE IS (LOGICAL(4))
  SELECT TYPE(M8=>T8)
  TYPE IS (LOGICAL(8))

  DO I = 1, 127
    IF (     SIZE(M1(I,:I),   KIND=M1%KIND )            .NE. I)         STOP 10
    IF (KIND(SIZE(M1(I,:I),   KIND=M1%KIND ))           .NE. M1%KIND)   STOP 11
    IF (     SIZE(M1(I:I,:I), KIND=M1%KIND )            .NE. I)         STOP 12
    IF (KIND(SIZE(M1(I:I,:I), KIND=M1%KIND ))           .NE. M1%KIND)   STOP 13
    IF (     SIZE(M1(I:I,:I), KIND=M1%KIND )            .NE. I)         STOP 14
    IF (KIND(SIZE(M1(I:I,:I), KIND=M1%KIND ))           .NE. M1%KIND)   STOP 15
    IF (     SIZE(M1(I:I,:I), KIND=M1%KIND, DIM=1 )     .NE. 1)         STOP 16
    IF (KIND(SIZE(M1(I:I,:I), KIND=M1%KIND, DIM=1 ))    .NE. M1%KIND)   STOP 17
    IF (     SIZE(M1(I:I,:I), KIND=M1%KIND, DIM=2 )     .NE. I)         STOP 18
    IF (KIND(SIZE(M1(I:I,:I), KIND=M1%KIND, DIM=2 ))    .NE. M1%KIND)   STOP 19
  END DO

  DO I = 1, 127
    IF (     SIZE(M2(I,:),   KIND=M2%KIND )            .NE. 129)              STOP 20
    IF (KIND(SIZE(M2(I,:),   KIND=M2%KIND ))           .NE. M2%KIND)          STOP 21
    IF (     SIZE(M2(I:,:),  KIND=M2%KIND )            .NE. (128-I)*129)      STOP 22
    IF (KIND(SIZE(M2(I:,:),  KIND=M2%KIND ))           .NE. M2%KIND)          STOP 23
    IF (     SIZE(M2(I:,I:), KIND=M2%KIND )            .NE. (128-I)*(130-I))  STOP 24
    IF (KIND(SIZE(M2(I:,I:), KIND=M2%KIND ))           .NE. M2%KIND)          STOP 25
    IF (     SIZE(M2(I:,I:), KIND=M2%KIND, DIM=1 )     .NE. (128-I))          STOP 26
    IF (KIND(SIZE(M2(I:,I:), KIND=M2%KIND, DIM=1 ))    .NE. M2%KIND)          STOP 27
    IF (     SIZE(M2(I:,I:), KIND=M2%KIND, DIM=2 )     .NE. (130-I))          STOP 28
    IF (KIND(SIZE(M2(I:,I:), KIND=M2%KIND, DIM=2 ))    .NE. M2%KIND)          STOP 29
  END DO

  DO I = 1, 127
    IF (     SIZE(M4(I,:),   KIND=M4%KIND )            .NE. 129)              STOP 40
    IF (KIND(SIZE(M4(I,:),   KIND=M4%KIND ))           .NE. M4%KIND)          STOP 41
    IF (     SIZE(M4(I:,:),  KIND=M4%KIND )            .NE. (128-I)*129)      STOP 42
    IF (KIND(SIZE(M4(I:,:),  KIND=M4%KIND ))           .NE. M4%KIND)          STOP 43
    IF (     SIZE(M4(I:,I:), KIND=M4%KIND )            .NE. (128-I)*(130-I))  STOP 44
    IF (KIND(SIZE(M4(I:,I:), KIND=M4%KIND ))           .NE. M4%KIND)          STOP 45
    IF (     SIZE(M4(I:,I:), KIND=M4%KIND, DIM=1 )     .NE. (128-I))          STOP 46
    IF (KIND(SIZE(M4(I:,I:), KIND=M4%KIND, DIM=1 ))    .NE. M4%KIND)          STOP 47
    IF (     SIZE(M4(I:,I:), KIND=M4%KIND, DIM=2 )     .NE. (130-I))          STOP 48
    IF (KIND(SIZE(M4(I:,I:), KIND=M4%KIND, DIM=2 ))    .NE. M4%KIND)          STOP 49
  END DO

  DO I = 1, 127
    IF (     SIZE(M8(I,:),   KIND=M8%KIND )            .NE. 129)              STOP 80
    IF (KIND(SIZE(M8(I,:),   KIND=M8%KIND ))           .NE. M8%KIND)          STOP 81
    IF (     SIZE(M8(I:,:),  KIND=M8%KIND )            .NE. (128-I)*129)      STOP 82
    IF (KIND(SIZE(M8(I:,:),  KIND=M8%KIND ))           .NE. M8%KIND)          STOP 83
    IF (     SIZE(M8(I:,I:), KIND=M8%KIND )            .NE. (128-I)*(130-I))  STOP 84
    IF (KIND(SIZE(M8(I:,I:), KIND=M8%KIND ))           .NE. M8%KIND)          STOP 85
    IF (     SIZE(M8(I:,I:), KIND=M8%KIND, DIM=1 )     .NE. (128-I))          STOP 86
    IF (KIND(SIZE(M8(I:,I:), KIND=M8%KIND, DIM=1 ))    .NE. M8%KIND)          STOP 87
    IF (     SIZE(M8(I:,I:), KIND=M8%KIND, DIM=2 )     .NE. (130-I))          STOP 88
    IF (KIND(SIZE(M8(I:,I:), KIND=M8%KIND, DIM=2 ))    .NE. M8%KIND)          STOP 89
  END DO


  CLASS DEFAULT
    STOP 92
  END SELECT
  CLASS DEFAULT
    STOP 92
  END SELECT
  CLASS DEFAULT
    STOP 93
  END SELECT
  CLASS DEFAULT
    STOP 94
  END SELECT

  END ASSOCIATE

  END



