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
!*  If KIND is present, the kind type parameter is that specified  by the value of KIND;
!*  otherwise the kind type parameter is that of default integer type
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLbound4
  IMPLICIT NONE

  INTEGER(1) :: I1
  INTEGER(4) :: I

  INTEGER, PARAMETER    :: L1=2**7-2
  INTEGER, PARAMETER    :: L2=2**7-1

  CHARACTER(128), PARAMETER :: CC0(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2)=" "
  CLASS(*), ALLOCATABLE     :: CC(:,:,:,:,:,:,:,:,:)

  ALLOCATE(CHARACTER(128) :: CC(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2))


  DO I1 = 1, 9
    IF (     UBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:8)))   .NE. L2)     ERROR STOP 11
    IF (KIND(UBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:8))))  .NE. 8)      ERROR STOP 12
    IF (     UBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:4)))   .NE. L2)     ERROR STOP 13
    IF (KIND(UBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:4))))  .NE. 4)      ERROR STOP 14
    IF (     UBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:2)))   .NE. L2)     ERROR STOP 15
    IF (KIND(UBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:2))))  .NE. 2)      ERROR STOP 16
    IF (     UBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:1)))   .NE. L2)     ERROR STOP 17
    IF (KIND(UBOUND(ARRAY=CC, DIM=I1, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:1))))  .NE. 1)      ERROR STOP 18
  END DO

  IF (ANY( UBOUND(ARRAY=CC, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:8)))   .NE. L2))              ERROR STOP 21
  IF (KIND(UBOUND(ARRAY=CC, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:8))))  .NE. 8)                ERROR STOP 22
  IF (ANY( UBOUND(ARRAY=CC, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:4)))   .NE. L2))              ERROR STOP 23
  IF (KIND(UBOUND(ARRAY=CC, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:4))))  .NE. 4)                ERROR STOP 24
  IF (ANY( UBOUND(ARRAY=CC, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:2)))   .NE. L2))              ERROR STOP 25
  IF (KIND(UBOUND(ARRAY=CC, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:2))))  .NE. 2)                ERROR STOP 26
  IF (ANY( UBOUND(ARRAY=CC, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:1)))   .NE. L2))              ERROR STOP 27
  IF (KIND(UBOUND(ARRAY=CC, KIND=LEN(CC0(:,:,:,:,:,:,:,:,:)(1:1))))  .NE. 1)                ERROR STOP 28

  IF (ANY( UBOUND(ARRAY=CC)   .NE. L2))                      ERROR STOP 31
  IF (KIND(UBOUND(ARRAY=CC))  .NE. 4)                        ERROR STOP 32

  END

