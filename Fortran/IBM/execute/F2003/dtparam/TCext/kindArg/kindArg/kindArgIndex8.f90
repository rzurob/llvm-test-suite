! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/kindArg/kindArg/kindArgIndex8.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
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
!*  Long string(2**30)
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIndex8
  IMPLICIT NONE

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  INTEGER   :: I, J
  CHARACTER(LEN=:), POINTER :: CC
  CHARACTER(LEN=128*100) :: C

  TYPE :: DT(N1,D1,D2,D3,D4)    ! (20,1,2,4,8)
    INTEGER, KIND :: D1,D2,D3,D4
    INTEGER, LEN  :: N1
    INTEGER(D1)   :: K1(2**10)=0
    INTEGER(D2)   :: K2(2**10)=0
    INTEGER(D3)   :: K4(2**10)=0
    INTEGER(D4)   :: K8(2**10)=0
  END TYPE

  TYPE (DT(20,1,2,4,8)), PARAMETER :: T(128)=DT(20,1,2,4,8)(1,2,4,8)

  DO I=1, LEN(C)
    C(I:I)= ACHAR(MOD(I,128))
  END DO

  ALLOCATE(CC, SOURCE=C)

  DO I = 1, LEN(C), 128
    IF (     INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.FALSE., KIND=KIND(T%K8(1)))   .NE. 1)               STOP 11
    IF (KIND(INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.FALSE., KIND=KIND(T%K8(1))))  .NE. 8)               STOP 12
    IF (     INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.FALSE., KIND=KIND(T%K4(1)))   .NE. 1)               STOP 13
    IF (KIND(INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.FALSE., KIND=KIND(T%K4(1))))  .NE. 4)               STOP 14
    IF (     INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.FALSE., KIND=KIND(T%K2(1)))   .NE. 1)               STOP 15
    IF (KIND(INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.FALSE., KIND=KIND(T%K2(1))))  .NE. 2)               STOP 16
    IF (     INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.FALSE., KIND=KIND(T%K1(1)))   .NE. 1)               STOP 17
    IF (KIND(INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.FALSE., KIND=KIND(T%K1(1))))  .NE. 1)               STOP 18
  END DO

  DO I = 1, LEN(C), 128
    IF (     INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.TRUE., KIND=KIND(T(1)%K8))   .NE. LEN(C)-I-126)    STOP 21
    IF (KIND(INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.TRUE., KIND=KIND(T(1)%K8)))  .NE. 8)               STOP 22
    IF (     INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.TRUE., KIND=KIND(T(1)%K4))   .NE. LEN(C)-I-126)    STOP 23
    IF (KIND(INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.TRUE., KIND=KIND(T(1)%K4)))  .NE. 4)               STOP 24
    IF (     INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.TRUE., KIND=KIND(T(1)%K2))   .NE. LEN(C)-I-126)    STOP 25
    IF (KIND(INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.TRUE., KIND=KIND(T(1)%K2)))  .NE. 2)               STOP 26
!   IF (     INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.TRUE., KIND=KIND(T(1)%K1))   .NE. LEN(C)-I-126)    STOP 27
    IF (KIND(INDEX(STRING=CC(I:), SUBSTRING=CC(1:128), BACK=.TRUE., KIND=KIND(T(1)%K1)))  .NE. 1)               STOP 28
  END DO



  END

