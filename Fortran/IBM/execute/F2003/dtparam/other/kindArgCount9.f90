!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgCount9
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
!*  -qintsize
!*
!*  (322580 with -qintsize=8)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  INTEGER :: I

  TYPE DT(K)
    INTEGER, KIND :: K
  END TYPE

  TYPE(DT(I%KIND)), PARAMETER :: T=DT(I%KIND)()


  LOGICAL(1) :: M1(126,126)=.FALSE.
  LOGICAL(2) :: M2(126,126)=.FALSE.
  LOGICAL(4) :: M4(126,126)=.FALSE.
  LOGICAL(8) :: M8(126,126)=.FALSE.

  END MODULE

  PROGRAM kindArgCount9
  USE M
  IMPLICIT NONE

  DO I = 1, 126
    M1(I,I) = .TRUE.
    M2(I,I) = .TRUE.
    M4(I,I) = .TRUE.
    M8(I,I) = .TRUE.
  END DO

  DO I = 1, 126
    IF (     COUNT(M1(:, I), KIND=KIND(T%K) )    .NE. 1)       STOP 11
    IF (KIND(COUNT(M1(:, I), KIND=KIND(T%K) ))   .NE. I%KIND)  STOP 12
    IF (     COUNT(M2(:, I), KIND=KIND(T%K) )    .NE. 1)       STOP 13
    IF (KIND(COUNT(M2(:, I), KIND=KIND(T%K) ))   .NE. I%KIND)  STOP 14
    IF (     COUNT(M4(:, I), KIND=KIND(T%K) )    .NE. 1)       STOP 15
    IF (KIND(COUNT(M4(:, I), KIND=KIND(T%K) ))   .NE. I%KIND)  STOP 16
    IF (     COUNT(M8(:, I), KIND=KIND(T%K) )    .NE. 1)       STOP 17
    IF (KIND(COUNT(M8(:, I), KIND=KIND(T%K) ))   .NE. I%KIND)  STOP 18
  END DO

  IF ( ANY( COUNT(M1, KIND=KIND(T%K), DIM=1 )    .NE. 1  ))    STOP 21
  IF (KIND(COUNT(M1, KIND=KIND(T%K),  DIM=1 ))   .NE. I%KIND)  STOP 22

  IF ( ANY( COUNT(M2, KIND=KIND(T%K), DIM=1 )    .NE. 1  ))    STOP 21
  IF (KIND(COUNT(M2, KIND=KIND(T%K),  DIM=1 ))   .NE. I%KIND)  STOP 22

  IF ( ANY( COUNT(M4, KIND=KIND(T%K), DIM=2 )    .NE. 1  ))    STOP 21
  IF (KIND(COUNT(M4, KIND=KIND(T%K),  DIM=2 ))   .NE. I%KIND)  STOP 22

  IF ( ANY( COUNT(M8, KIND=KIND(T%K), DIM=2 )    .NE. 1  ))    STOP 21
  IF (KIND(COUNT(M8, KIND=KIND(T%K),  DIM=2 ))   .NE. I%KIND)  STOP 22


  END

