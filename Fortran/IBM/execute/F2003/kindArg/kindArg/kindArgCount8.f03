!*********************************************************************
!*  ===================================================================
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
!*  entities with different attubute used for kind arg - dummy
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  IMPLICIT NONE

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

  PROGRAM kindArgCount8
  USE M

  DO I = 1, 126
    M1(I,I) = .TRUE.
    M2(I,I) = .TRUE.
    M4(I,I) = .TRUE.
    M8(I,I) = .TRUE.
  END DO

  CALL IntSub(1_1,2_2,4_4,8_8)

  CONTAINS

  SUBROUTINE IntSub(K1,K2,K4,K8)
  INTEGER(1), INTENT(IN), OPTIONAL :: K1
  INTEGER(2), INTENT(IN), OPTIONAL :: K2
  INTEGER(4), INTENT(IN), OPTIONAL :: K4
  INTEGER(8), INTENT(IN), OPTIONAL :: K8


  DO I1 = 1, 126
    IF (     COUNT(M1(I1,:), KIND=KIND(K1) )    .NE. 1)   ERROR STOP 11
    IF (KIND(COUNT(M1(I1,:), KIND=KIND(K1) ))   .NE. K1)  ERROR STOP 12
    IF (     COUNT(M1(I1,:), KIND=KIND(K2) )    .NE. 1)   ERROR STOP 13
    IF (KIND(COUNT(M1(I1,:), KIND=KIND(K2) ))   .NE. K2)  ERROR STOP 14
    IF (     COUNT(M1(I1,:), KIND=KIND(K4) )    .NE. 1)   ERROR STOP 15
    IF (KIND(COUNT(M1(I1,:), KIND=KIND(K4) ))   .NE. K4)  ERROR STOP 16
    IF (     COUNT(M1(I1,:), KIND=KIND(K8) )    .NE. 1)   ERROR STOP 17
    IF (KIND(COUNT(M1(I1,:), KIND=KIND(K8) ))   .NE. K8)  ERROR STOP 18
  END DO

  DO I2 = 1, 126
    IF (     COUNT(M2(I2,:), KIND=KIND(K1) )    .NE. 1)   ERROR STOP 21
    IF (KIND(COUNT(M2(I2,:), KIND=KIND(K1) ))   .NE. K1)  ERROR STOP 22
    IF (     COUNT(M2(I2,:), KIND=KIND(K2) )    .NE. 1)   ERROR STOP 23
    IF (KIND(COUNT(M2(I2,:), KIND=KIND(K2) ))   .NE. K2)  ERROR STOP 24
    IF (     COUNT(M2(I2,:), KIND=KIND(K4) )    .NE. 1)   ERROR STOP 25
    IF (KIND(COUNT(M2(I2,:), KIND=KIND(K4) ))   .NE. K4)  ERROR STOP 26
    IF (     COUNT(M2(I2,:), KIND=KIND(K8) )    .NE. 1)   ERROR STOP 27
    IF (KIND(COUNT(M2(I2,:), KIND=KIND(K8) ))   .NE. K8)  ERROR STOP 28
  END DO

  DO I4 = 1, 126
    IF (     COUNT(M4(I4,:), KIND=KIND(K1) )    .NE. 1)   ERROR STOP 41
    IF (KIND(COUNT(M4(I4,:), KIND=KIND(K1) ))   .NE. K1)  ERROR STOP 42
    IF (     COUNT(M4(I4,:), KIND=KIND(K2) )    .NE. 1)   ERROR STOP 43
    IF (KIND(COUNT(M4(I4,:), KIND=KIND(K2) ))   .NE. K2)  ERROR STOP 44
    IF (     COUNT(M4(I4,:), KIND=KIND(K4) )    .NE. 1)   ERROR STOP 45
    IF (KIND(COUNT(M4(I4,:), KIND=KIND(K4) ))   .NE. K4)  ERROR STOP 46
    IF (     COUNT(M4(I4,:), KIND=KIND(K8) )    .NE. 1)   ERROR STOP 47
    IF (KIND(COUNT(M4(I4,:), KIND=KIND(K8) ))   .NE. K8)  ERROR STOP 48
  END DO

  DO I8 = 1, 126
    IF (     COUNT(M8(I8,:), KIND=KIND(K1) )    .NE. 1)   ERROR STOP 81
    IF (KIND(COUNT(M8(I8,:), KIND=KIND(K1) ))   .NE. K1)  ERROR STOP 82
    IF (     COUNT(M8(I8,:), KIND=KIND(K2) )    .NE. 1)   ERROR STOP 83
    IF (KIND(COUNT(M8(I8,:), KIND=KIND(K2) ))   .NE. K2)  ERROR STOP 84
    IF (     COUNT(M8(I8,:), KIND=KIND(K4) )    .NE. 1)   ERROR STOP 85
    IF (KIND(COUNT(M8(I8,:), KIND=KIND(K4) ))   .NE. K4)  ERROR STOP 86
    IF (     COUNT(M8(I8,:), KIND=KIND(K8) )    .NE. 1)   ERROR STOP 87
    IF (KIND(COUNT(M8(I8,:), KIND=KIND(K8) ))   .NE. K8)  ERROR STOP 88
  END DO


  END SUBROUTINE

  END
