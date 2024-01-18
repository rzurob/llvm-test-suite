!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgCount6
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : COUNT
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
!*  Entities with different attubute used for kind arg - associate/select type
!*
!*  (322578)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgCount6
  IMPLICIT NONE
  TYPE DT
    INTEGER(1) :: K1
    INTEGER(2) :: K2
    INTEGER(4) :: K4
    INTEGER(8) :: K8
  END TYPE

  TYPE(DT), PARAMETER :: T=DT(1,2,4,8)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8
  INTEGER I

  CLASS(*), ALLOCATABLE :: MM1(:,:)
  CLASS(*), POINTER     :: MM2(:,:)
  CLASS(*), ALLOCATABLE :: MM4(:,:)
  CLASS(*), POINTER     :: MM8(:,:)

  ALLOCATE(MM1(126,126) ,SOURCE=.FALSE._1)
  ALLOCATE(MM2(126,126) ,SOURCE=.FALSE._2)
  ALLOCATE(MM4(126,126) ,SOURCE=.FALSE._4)
  ALLOCATE(MM8(126,126) ,SOURCE=.FALSE._8)

  SELECT TYPE(MM1)
  TYPE IS (LOGICAL(1))
  SELECT TYPE(MM2)
  TYPE IS (LOGICAL(2))
  SELECT TYPE(MM4)
  TYPE IS (LOGICAL(4))
  SELECT TYPE(MM8)
  TYPE IS (LOGICAL(8))
    DO I = 1, 126
      MM1(I,I) = .TRUE.
      MM2(I,I) = .TRUE.
      MM4(I,I) = .TRUE.
      MM8(I,I) = .TRUE.
    END DO
  END SELECT
  END SELECT
  END SELECT
  END SELECT


  ASSOCIATE (T1=>MM1, T2=>MM2, T4=>MM4, T8=>MM8)
  SELECT TYPE(M1=>T1)
  TYPE IS (LOGICAL(1))
  SELECT TYPE(M2=>T2)
  TYPE IS (LOGICAL(2))
  SELECT TYPE(M4=>T4)
  TYPE IS (LOGICAL(4))
  SELECT TYPE(M8=>T8)
  TYPE IS (LOGICAL(8))

  DO I1 = 1, 126
    IF (     COUNT(M1(I1,:), KIND=T%K1%KIND )               .NE. 1)          STOP 11
    IF (KIND(COUNT(M1(I1,:), KIND=T%K1%KIND ))              .NE. T%K1%KIND)  STOP 12
    IF (     COUNT(M1(I1,:) )                               .NE. 1)          STOP 13
    IF (KIND(COUNT(M1(I1,:)))                               .NE. 4)          STOP 14
    IF (ANY (COUNT(M1, KIND=T%K1%KIND, DIM=MOD(I1,2_1)+1 )  .NE. 1))         STOP 15
    IF (KIND(COUNT(M1, KIND=T%K1%KIND, DIM=MOD(I1,2_1)+1 )) .NE. T%K1%KIND)  STOP 16
    IF (ANY (COUNT(M1, DIM=MOD(I1,2_1)+1 )                  .NE. 1))         STOP 17
    IF (KIND(COUNT(M1, DIM=MOD(I1,2_1)+1))                  .NE. 4)          STOP 18
  END DO

  DO I2 = 1, 126
    IF (     COUNT(M2(I2,:), KIND=T%K8%KIND )               .NE. 1)          STOP 21
    IF (KIND(COUNT(M2(I2,:), KIND=T%K8%KIND ))              .NE. T%K8%KIND)  STOP 22
    IF (     COUNT(M2(I2,:) )                               .NE. 1)          STOP 23
    IF (KIND(COUNT(M2(I2,:)))                               .NE. 4)          STOP 24
    IF (ANY (COUNT(M2, KIND=T%K4%KIND, DIM=MOD(I2,2_2)+1 )  .NE. 1))         STOP 25
    IF (KIND(COUNT(M2, KIND=T%K4%KIND, DIM=MOD(I2,2_2)+1 )) .NE. T%K4%KIND)  STOP 26
    IF (ANY (COUNT(M2, DIM=MOD(I2,2_2)+1 )                  .NE. 1))         STOP 27
    IF (KIND(COUNT(M2, DIM=MOD(I2,2_2)+1))                  .NE. 4)          STOP 28
  END DO

  DO I4 = 1, 126
    IF (     COUNT(M4(I4,:), KIND=T%K1%KIND )               .NE. 1)          STOP 41
    IF (KIND(COUNT(M4(I4,:), KIND=T%K1%KIND ))              .NE. T%K1%KIND)  STOP 42
    IF (     COUNT(M4(I4,:) )                               .NE. 1)          STOP 43
    IF (KIND(COUNT(M4(I4,:)))                               .NE. 4)          STOP 44
    IF (ANY (COUNT(M4, KIND=T%K2%KIND, DIM=MOD(I4,2_4)+1 )  .NE. 1))         STOP 45
    IF (KIND(COUNT(M4, KIND=T%K2%KIND, DIM=MOD(I4,2_4)+1 )) .NE. T%K2%KIND)  STOP 46
    IF (ANY (COUNT(M4, DIM=MOD(I4,2_4)+1 )                  .NE. 1))         STOP 47
    IF (KIND(COUNT(M4, DIM=MOD(I4,2_4)+1))                  .NE. 4)          STOP 48
  END DO

  DO I8 = 1, 126
    IF (     COUNT(M8(I8,:), KIND=T%K1%KIND )               .NE. 1)          STOP 81
    IF (KIND(COUNT(M8(I8,:), KIND=T%K1%KIND ))              .NE. T%K1%KIND)  STOP 82
    IF (     COUNT(M8(I8,:) )                               .NE. 1)          STOP 83
    IF (KIND(COUNT(M8(I8,:)))                               .NE. 4)          STOP 84
    IF (ANY (COUNT(M8, KIND=T%K2%KIND, DIM=MOD(I8,2_8)+1 )  .NE. 1))         STOP 85
    IF (KIND(COUNT(M8, KIND=T%K2%KIND, DIM=MOD(I8,2_8)+1 )) .NE. T%K2%KIND)  STOP 86
    IF (ANY (COUNT(M8, DIM=MOD(I8,2_8)+1 )                  .NE. 1))         STOP 87
    IF (KIND(COUNT(M8, DIM=MOD(I8,2_8)+1))                  .NE. 4)          STOP 88
  END DO

  CLASS DEFAULT
    STOP 91
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

