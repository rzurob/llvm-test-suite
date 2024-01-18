!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 05, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : UBOUND
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
!*  Returns all the upper bounds of an array or a specified upper bound.
!*
!*  (322388)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIUbound3
  IMPLICIT NONE

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4, I
  INTEGER(8) :: I8

  CHARACTER(:), ALLOCATABLE    :: CC(:,:,:,:,:,:,:,:,:)
  CHARACTER(128)    :: CC1(1:1,2:2,3:3,4:4,5:5,6:6,7:7,8:8,9:9)
  INTEGER           :: II(9) = (/(I, I=1,9)/)

  ALLOCATE(CHARACTER(128) :: CC(1:1,2:2,3:3,4:4,5:5,6:6,7:7,8:8,9:9))
  DO I=1, 128
    CC(1,2,3,4,5,6,7,8,9)(I:I) = ACHAR(I)
  END DO

  DO I1 = 1, 9
    IF (UBOUND(ARRAY=CC, DIM=I1, KIND=1_1 ) .NE. I1) STOP 11
    IF (UBOUND(ARRAY=CC, DIM=I1, KIND=1_2 ) .NE. I1) STOP 12
    IF (UBOUND(ARRAY=CC, DIM=I1, KIND=1_4 ) .NE. I1) STOP 13
    IF (UBOUND(ARRAY=CC, DIM=I1, KIND=1_8 ) .NE. I1) STOP 14
  END DO

  DO I2 = 1, 9
    IF (UBOUND(ARRAY=CC, DIM=I2, KIND=1_1 ) .NE. I2) STOP 21
    IF (UBOUND(ARRAY=CC, DIM=I2, KIND=1_2 ) .NE. I2) STOP 22
    IF (UBOUND(ARRAY=CC, DIM=I2, KIND=1_4 ) .NE. I2) STOP 23
    IF (UBOUND(ARRAY=CC, DIM=I2, KIND=1_8 ) .NE. I2) STOP 24
  END DO

  DO I4 = 1, 9
    IF (UBOUND(ARRAY=CC, DIM=I4, KIND=1_1 ) .NE. I4) STOP 41
    IF (UBOUND(ARRAY=CC, DIM=I4, KIND=1_2 ) .NE. I4) STOP 42
    IF (UBOUND(ARRAY=CC, DIM=I4, KIND=1_4 ) .NE. I4) STOP 43
    IF (UBOUND(ARRAY=CC, DIM=I4, KIND=1_8 ) .NE. I4) STOP 44
  END DO

  DO I8 = 1, 9
    IF (UBOUND(ARRAY=CC, DIM=I8, KIND=1_1 ) .NE. I8) STOP 81
    IF (UBOUND(ARRAY=CC, DIM=I8, KIND=1_2 ) .NE. I8) STOP 82
    IF (UBOUND(ARRAY=CC, DIM=I8, KIND=1_4 ) .NE. I8) STOP 83
    IF (UBOUND(ARRAY=CC, DIM=I8, KIND=1_8 ) .NE. I8) STOP 84
  END DO

  IF (ANY(UBOUND(ARRAY=CC, KIND=1_1 ) .NE. II)) STOP 91
  IF (ANY(UBOUND(ARRAY=CC)            .NE. II)) STOP 92

  IF (ANY(UBOUND(ARRAY=UBOUND(ARRAY=CC,KIND=1_1), KIND=8_1 ) .NE. 9)) STOP 93
  IF (ANY(UBOUND(ARRAY=UBOUND(ARRAY=CC,KIND=8_1), KIND=1_1 ) .NE. 9)) STOP 94


  END

