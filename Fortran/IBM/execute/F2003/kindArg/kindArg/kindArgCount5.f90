!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgCount5
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
!*  Result Characteristics.
!*  Integer. If KIND is present, the kind type parameter is that specified by the value of KIND;
!*  otherwise the kind type parameter is that of default integer type.
!*  The result is scalar if DIM is absent; otherwise, the result has rank n - 1 and shape (d1, d2,
!*  ..., dDIM-1, dDIM+1, ..., dn) where (d1, d2, ..., dn) is the shape of MASK.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgCount5


  INTEGER(1), ALLOCATABLE ::  K1
  INTEGER(2), POINTER     ::  K2
  INTEGER(4), ALLOCATABLE ::  K4
  INTEGER(8), POINTER     ::  K8

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  LOGICAL(1) :: M1(126)=(/(.TRUE., .FALSE., I=1,126,2)/)
  LOGICAL(2) :: M2(126)=(/(.TRUE., .FALSE., I=1,126,2)/)
  LOGICAL(4) :: M4(126)=(/(.TRUE., .FALSE., I=1,126,2)/)
  LOGICAL(8) :: M8(126)=(/(.TRUE., .FALSE., I=1,126,2)/)


  ALLOCATE(K1, SOURCE=1_1)
  ALLOCATE(K2, SOURCE=1_2)
  ALLOCATE(K4, SOURCE=1_4)
  ALLOCATE(K8, SOURCE=1_8)


  DO I1 = 1,126
    IF (     COUNT(M1(:I1), KIND=K1%KIND )            .NE. (I1+1)/2)       STOP 11
    IF (KIND(COUNT(M1(:I1), KIND=K1%KIND ))           .NE. K1%KIND)        STOP 12
    IF (     COUNT(M1(:I1) )                          .NE. (I1+1)/2)       STOP 13
    IF (KIND(COUNT(M1(:I1)))                          .NE. 4)              STOP 14
    IF (     COUNT(M1(:I1), KIND=K1%KIND, DIM=1_1 )   .NE. (I1+1)/2)       STOP 15
    IF (KIND(COUNT(M1(:I1), KIND=K1%KIND, DIM=1_2 ))  .NE. K1%KIND)        STOP 16
    IF (     COUNT(M1(:I1), DIM=1_8 )                 .NE. (I1+1)/2)       STOP 17
    IF (KIND(COUNT(M1(:I1), DIM=1_8))                 .NE. 4)              STOP 18
  END DO

  DO I2 = 1,126
    IF (     COUNT(M2(:I2), KIND=K2%KIND )            .NE. (I2+1)/2)       STOP 21
    IF (KIND(COUNT(M2(:I2), KIND=K2%KIND ))           .NE. K2%KIND)        STOP 22
    IF (     COUNT(M2(:I2) )                          .NE. (I2+1)/2)       STOP 23
    IF (KIND(COUNT(M2(:I2)))                          .NE. 4)              STOP 24
    IF (     COUNT(M2(:I2), KIND=K2%KIND, DIM=1_4 )   .NE. (I2+1)/2)       STOP 25
    IF (KIND(COUNT(M2(:I2), KIND=K2%KIND, DIM=1_1 ))  .NE. K2%KIND)        STOP 26
    IF (     COUNT(M2(:I2), DIM=1_4 )                 .NE. (I2+1)/2)       STOP 27
    IF (KIND(COUNT(M2(:I2), DIM=1_4))                 .NE. 4)              STOP 28
  END DO

  DO I4 = 1,126
    IF (     COUNT(M4(:I4), KIND=K4%KIND )            .NE. (I4+1)/2)       STOP 41
    IF (KIND(COUNT(M4(:I4), KIND=K4%KIND ))           .NE. K4%KIND)        STOP 42
    IF (     COUNT(M4(:I4) )                          .NE. (I4+1)/2)       STOP 43
    IF (KIND(COUNT(M4(:I4)))                          .NE. 4)              STOP 44
    IF (     COUNT(M4(:I4), KIND=K4%KIND, DIM=1_8 )   .NE. (I4+1)/2)       STOP 45
    IF (KIND(COUNT(M4(:I4), KIND=K4%KIND, DIM=1_8 ))  .NE. K4%KIND)        STOP 46
    IF (     COUNT(M4(:I4), DIM=1_2 )                 .NE. (I4+1)/2)       STOP 47
    IF (KIND(COUNT(M4(:I4), DIM=1_2))                 .NE. 4)              STOP 48
  END DO

  DO I8 = 1,126
    IF (     COUNT(M8(:I8), KIND=K8%KIND )            .NE. (I8+1)/2)       STOP 81
    IF (KIND(COUNT(M8(:I8), KIND=K8%KIND ))           .NE. K8%KIND)        STOP 82
    IF (     COUNT(M8(:I8) )                          .NE. (I8+1)/2)       STOP 83
    IF (KIND(COUNT(M8(:I8)))                          .NE. 4)              STOP 84
    IF (     COUNT(M8(:I8), KIND=K8%KIND, DIM=1_4 )   .NE. (I8+1)/2)       STOP 85
    IF (KIND(COUNT(M8(:I8), KIND=K8%KIND, DIM=1_1 ))  .NE. K8%KIND)        STOP 86
    IF (     COUNT(M8(:I8), DIM=1_8 )                 .NE. (I8+1)/2)       STOP 87
    IF (KIND(COUNT(M8(:I8), DIM=1_8))                 .NE. 4)              STOP 88
  END DO


  END

