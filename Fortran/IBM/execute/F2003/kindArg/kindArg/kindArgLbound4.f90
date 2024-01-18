!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 21, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : LBOUND
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
!*  If KIND is present, the kind type parameter is that specified  by the value of KIND;
!*  otherwise the kind type parameter is that of default integer type
!*
!*  (324733)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLbound4
  IMPLICIT NONE

  INTEGER(1) :: I1
  INTEGER(4) :: I

  INTEGER    :: L1=2**7-1
  INTEGER    :: L2=2**7

  CHARACTER(128) :: C
  CHARACTER(128), POINTER :: CC(:,:,:,:,:,:,:,:,:)

  ALLOCATE(CHARACTER(128) :: CC(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2))

  DO I=1,128
    C(I:I)=ACHAR(I)
    CC(L1,L1,L1,L1,L1,L1,L1,L1,L1)(I:I)=ACHAR(I)
    CC(L2,L2,L2,L2,L2,L2,L2,L2,L2)(I:I)=ACHAR(I)
  END DO


  DO I1 = 1, 9
    IF (     LBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:8)))   .NE. L1)               ERROR STOP 11
    IF (KIND(LBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:8))))  .NE. 8)                ERROR STOP 12
    IF (     LBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:4)))   .NE. L1)               ERROR STOP 13
    IF (KIND(LBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:4))))  .NE. 4)                ERROR STOP 14
    IF (     LBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:2)))   .NE. L1)               ERROR STOP 15
    IF (KIND(LBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:2))))  .NE. 2)                ERROR STOP 16
    IF (     LBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:1)))   .NE. L1)               ERROR STOP 17
    IF (KIND(LBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:1))))  .NE. 1)                ERROR STOP 18
  END DO

  IF (ANY( LBOUND(ARRAY=CC, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:8)))   .NE. L1))              ERROR STOP 21
  IF (KIND(LBOUND(ARRAY=CC, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:8))))  .NE. 8)                ERROR STOP 22
  IF (ANY( LBOUND(ARRAY=CC, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:4)))   .NE. L1))              ERROR STOP 23
  IF (KIND(LBOUND(ARRAY=CC, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:4))))  .NE. 4)                ERROR STOP 24
  IF (ANY( LBOUND(ARRAY=CC, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:2)))   .NE. L1))              ERROR STOP 25
  IF (KIND(LBOUND(ARRAY=CC, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:2))))  .NE. 2)                ERROR STOP 26
  IF (ANY( LBOUND(ARRAY=CC, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:1)))   .NE. L1))              ERROR STOP 27
  IF (KIND(LBOUND(ARRAY=CC, KIND=LEN(CC(:,:,:,:,:,:,:,:,:)(1:1))))  .NE. 1)                ERROR STOP 28

  IF (ANY( LBOUND(ARRAY=CC)   .NE. L1))                      ERROR STOP 31
  IF (KIND(LBOUND(ARRAY=CC))  .NE. 4)                        ERROR STOP 32

  END

