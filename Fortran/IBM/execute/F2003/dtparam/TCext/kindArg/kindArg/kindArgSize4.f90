! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/kindArg/kindArg/kindArgSize4.f
! opt variations: -qnok -ql

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
!*  Size the number of true elements of ARRAY along dimension DIM
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgSize4
  IMPLICIT NONE

  CLASS(*), ALLOCATABLE    :: CC(:,:,:,:,:,:,:,:,:)

  TYPE :: DT(D1)    ! (4)
      INTEGER, KIND :: D1
  END TYPE

  INTEGER :: I

  ALLOCATE(CHARACTER(128) :: CC(1:1,2:2,3:3,4:4,5:5,6:6,7:7,8:8,9:9))

  DO I =1, 9
    IF (SIZE(CC, KIND=1_1, DIM=I ) .NE. 1 ) ERROR STOP 11
    IF (SIZE(CC, KIND=1_2, DIM=I ) .NE. 1 ) ERROR STOP 12
    IF (SIZE(CC, KIND=1_4, DIM=I ) .NE. 1 ) ERROR STOP 13
    IF (SIZE(CC, KIND=1_8, DIM=I ) .NE. 1 ) ERROR STOP 14
  END DO

  IF (SIZE(ARRAY=CC, KIND=1_1 ) .NE. 1 ) ERROR STOP 21
  IF (SIZE(ARRAY=CC, KIND=2_2 ) .NE. 1 ) ERROR STOP 22
  IF (SIZE(ARRAY=CC, KIND=4_4 ) .NE. 1 ) ERROR STOP 23
  IF (SIZE(ARRAY=CC, KIND=8_8 ) .NE. 1 ) ERROR STOP 24

  DEALLOCATE(CC)
  ALLOCATE(COMPLEX(16) :: CC(0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0))

  DO I =1, 9
    IF (SIZE(CC, KIND=1_1, DIM=I ) .NE. 1 ) ERROR STOP 31
    IF (SIZE(CC, KIND=1_2, DIM=I ) .NE. 1 ) ERROR STOP 32
    IF (SIZE(CC, KIND=1_4, DIM=I ) .NE. 1 ) ERROR STOP 33
    IF (SIZE(CC, KIND=1_8, DIM=I ) .NE. 1 ) ERROR STOP 34
  END DO

  IF (SIZE(ARRAY=CC, KIND=1_1 ) .NE. 1 ) ERROR STOP 41
  IF (SIZE(ARRAY=CC, KIND=2_2 ) .NE. 1 ) ERROR STOP 42
  IF (SIZE(ARRAY=CC, KIND=4_4 ) .NE. 1 ) ERROR STOP 43
  IF (SIZE(ARRAY=CC, KIND=8_8 ) .NE. 1 ) ERROR STOP 44

  DEALLOCATE(CC)
  ALLOCATE(DT(4) :: CC(-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0))

  DO I =1, 9
    IF (SIZE(CC, KIND=1_1, DIM=I ) .NE. 2 ) ERROR STOP 51
    IF (SIZE(CC, KIND=1_2, DIM=I ) .NE. 2 ) ERROR STOP 52
    IF (SIZE(CC, KIND=1_4, DIM=I ) .NE. 2 ) ERROR STOP 53
    IF (SIZE(CC, KIND=1_8, DIM=I ) .NE. 2 ) ERROR STOP 54
  END DO

!  IF (SIZE(ARRAY=CC, KIND=1_1 ) .NE. 2**9 ) ERROR STOP 61
  IF (SIZE(ARRAY=CC, KIND=2_2 ) .NE. 2**9 ) ERROR STOP 62
  IF (SIZE(ARRAY=CC, KIND=4_4 ) .NE. 2**9 ) ERROR STOP 63
  IF (SIZE(ARRAY=CC, KIND=8_8 ) .NE. 2**9 ) ERROR STOP 64

  END

