! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/kindArg/kindArg/kindArgCount7.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgCount7
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
!*  Entities with different attubute used for kind arg - function return
!*
!*  (322580)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  IMPLICIT NONE

  TYPE DT(D1,D2,D3,D4)    ! (1,2,4,8)
    INTEGER, KIND :: D1,D2,D3,D4
    INTEGER(D1)   :: K1=1
    INTEGER(D2)   :: K2=2
    INTEGER(D3)   :: K4=4
    INTEGER(D4)   :: K8=8
  END TYPE

  TYPE(DT(1,2,4,8)), PARAMETER :: T=DT(1,2,4,8)()

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8
  INTEGER I

  LOGICAL(1) :: M1(126,126)=.FALSE._1
  LOGICAL(2) :: M2(126,126)=.FALSE._2
  LOGICAL(4) :: M4(126,126)=.FALSE._4
  LOGICAL(8) :: M8(126,126)=.FALSE._8

  END MODULE

  PROGRAM kindArgCount7
  USE M

  DO I = 1, 126
    M1(I,I) = .TRUE.
    M2(I,I) = .TRUE.
    M4(I,I) = .TRUE.
    M8(I,I) = .TRUE.
  END DO

  DO I1 = 1, 126
    IF (     COUNT(M1(I1,:), KIND=KIND(T%K1) )    .NE. 1)     STOP 11
    IF (KIND(COUNT(M1(I1,:), KIND=KIND(T%K1) ))   .NE. T%K1)  STOP 12
    IF (     COUNT(M1(I1,:), KIND=KIND(T%K2) )    .NE. 1)     STOP 13
    IF (KIND(COUNT(M1(I1,:), KIND=KIND(T%K2) ))   .NE. T%K2)  STOP 14
    IF (     COUNT(M1(I1,:), KIND=KIND(T%K4) )    .NE. 1)     STOP 15
    IF (KIND(COUNT(M1(I1,:), KIND=KIND(T%K4) ))   .NE. T%K4)  STOP 16
    IF (     COUNT(M1(I1,:), KIND=KIND(T%K8) )    .NE. 1)     STOP 17
    IF (KIND(COUNT(M1(I1,:), KIND=KIND(T%K8) ))   .NE. T%K8)  STOP 18
  END DO

  DO I2 = 1, 126
    IF (     COUNT(M2(I2,:), KIND=KIND(T%K1) )    .NE. 1)     STOP 21
    IF (KIND(COUNT(M2(I2,:), KIND=KIND(T%K1) ))   .NE. T%K1)  STOP 22
    IF (     COUNT(M2(I2,:), KIND=KIND(T%K2) )    .NE. 1)     STOP 23
    IF (KIND(COUNT(M2(I2,:), KIND=KIND(T%K2) ))   .NE. T%K2)  STOP 24
    IF (     COUNT(M2(I2,:), KIND=KIND(T%K4) )    .NE. 1)     STOP 25
    IF (KIND(COUNT(M2(I2,:), KIND=KIND(T%K4) ))   .NE. T%K4)  STOP 26
    IF (     COUNT(M2(I2,:), KIND=KIND(T%K8) )    .NE. 1)     STOP 27
    IF (KIND(COUNT(M2(I2,:), KIND=KIND(T%K8) ))   .NE. T%K8)  STOP 28
  END DO

  DO I4 = 1, 126
    IF (     COUNT(M4(I4,:), KIND=KIND(T%K1) )    .NE. 1)     STOP 41
    IF (KIND(COUNT(M4(I4,:), KIND=KIND(T%K1) ))   .NE. T%K1)  STOP 42
    IF (     COUNT(M4(I4,:), KIND=KIND(T%K2) )    .NE. 1)     STOP 43
    IF (KIND(COUNT(M4(I4,:), KIND=KIND(T%K2) ))   .NE. T%K2)  STOP 44
    IF (     COUNT(M4(I4,:), KIND=KIND(T%K4) )    .NE. 1)     STOP 45
    IF (KIND(COUNT(M4(I4,:), KIND=KIND(T%K4) ))   .NE. T%K4)  STOP 46
    IF (     COUNT(M4(I4,:), KIND=KIND(T%K8) )    .NE. 1)     STOP 47
    IF (KIND(COUNT(M4(I4,:), KIND=KIND(T%K8) ))   .NE. T%K8)  STOP 48
  END DO

  DO I8 = 1, 126
    IF (     COUNT(M8(I8,:), KIND=KIND(T%K1) )    .NE. 1)     STOP 81
    IF (KIND(COUNT(M8(I8,:), KIND=KIND(T%K1) ))   .NE. T%K1)  STOP 82
    IF (     COUNT(M8(I8,:), KIND=KIND(T%K2) )    .NE. 1)     STOP 83
    IF (KIND(COUNT(M8(I8,:), KIND=KIND(T%K2) ))   .NE. T%K2)  STOP 84
    IF (     COUNT(M8(I8,:), KIND=KIND(T%K4) )    .NE. 1)     STOP 85
    IF (KIND(COUNT(M8(I8,:), KIND=KIND(T%K4) ))   .NE. T%K4)  STOP 86
    IF (     COUNT(M8(I8,:), KIND=KIND(T%K8) )    .NE. 1)     STOP 87
    IF (KIND(COUNT(M8(I8,:), KIND=KIND(T%K8) ))   .NE. T%K8)  STOP 88
  END DO



  END

