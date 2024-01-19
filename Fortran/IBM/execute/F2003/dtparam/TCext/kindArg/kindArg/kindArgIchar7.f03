! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/kindArg/kindArg/kindArgIchar7.f
! opt variations: -ql -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ICHAR
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
!*  Entities with different attribute used for kind arg - function return
!*
!*  (326109)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIchar7


  INTEGER(1), ALLOCATABLE :: I1, II1(:), K1
  INTEGER(2), POINTER     :: I2, II2(:), K2
  INTEGER(4), ALLOCATABLE :: I4, II4(:), K4
  INTEGER(8), POINTER     :: I8, II8(:), K8

  CHARACTER(:), ALLOCATABLE :: CC

  TYPE :: DT(D1)    ! (1)
    INTEGER, KIND :: D1
    INTEGER(D1)   :: K1=0
    INTEGER(D1)   :: K2=0
    INTEGER(D1)   :: K4=0
    INTEGER(D1)   :: K8=0
  END TYPE

  TYPE (DT(1)), PARAMETER :: T=DT(1)(1,2,4,8)

  ALLOCATE(I1)
  ALLOCATE(I2)
  ALLOCATE(I4)
  ALLOCATE(I8)

  ALLOCATE(II1(128))

  ALLOCATE(CHARACTER(128)::CC)
  DO I=1, 128
    CC(I:I)=CHAR(I=I-1, KIND=1)
  END DO

  DO I1 = 1, 127
    IF (ICHAR(CC(I1:I1), KIND=ICHAR(CHAR(1, KIND=SIZE((/T%K1/))), KIND=T%K8))  .NE. I1-1 ) ERROR STOP 11
    IF (ICHAR(CC(I1:I1), KIND=ICHAR(CHAR(1, KIND=SIZE((/T%K2/))), KIND=T%K4))  .NE. I1-1 ) ERROR STOP 12
    IF (ICHAR(CC(I1:I1), KIND=ICHAR(CHAR(1, KIND=SIZE((/T%K4/))), KIND=T%K2))  .NE. I1-1 ) ERROR STOP 13
    IF (ICHAR(CC(I1:I1), KIND=ICHAR(CHAR(1, KIND=SIZE((/T%K8/))), KIND=T%K1))  .NE. I1-1 ) ERROR STOP 14
  END DO

  DO I2 = 1, 128
    IF (ICHAR(CC(I2:I2), KIND=ICHAR(CHAR(2, KIND=SIZE((/T%K1/))), KIND=T%K8))  .NE. I2-1 ) ERROR STOP 21
    IF (ICHAR(CC(I2:I2), KIND=ICHAR(CHAR(2, KIND=SIZE((/T%K2/))), KIND=T%K4))  .NE. I2-1 ) ERROR STOP 22
    IF (ICHAR(CC(I2:I2), KIND=ICHAR(CHAR(2, KIND=SIZE((/T%K4/))), KIND=T%K2))  .NE. I2-1 ) ERROR STOP 23
    IF (ICHAR(CC(I2:I2), KIND=ICHAR(CHAR(2, KIND=SIZE((/T%K8/))), KIND=T%K1))  .NE. I2-1 ) ERROR STOP 24
  END DO

  DO I4 = 1, 128
    IF (ICHAR(CC(I4:I4), KIND=ICHAR(CHAR(4, KIND=SIZE((/T%K1/))), KIND=T%K8))  .NE. I4-1 ) ERROR STOP 41
    IF (ICHAR(CC(I4:I4), KIND=ICHAR(CHAR(4, KIND=SIZE((/T%K2/))), KIND=T%K4))  .NE. I4-1 ) ERROR STOP 42
    IF (ICHAR(CC(I4:I4), KIND=ICHAR(CHAR(4, KIND=SIZE((/T%K4/))), KIND=T%K2))  .NE. I4-1 ) ERROR STOP 43
    IF (ICHAR(CC(I4:I4), KIND=ICHAR(CHAR(4, KIND=SIZE((/T%K8/))), KIND=T%K1))  .NE. I4-1 ) ERROR STOP 44
  END DO

  DO I8 = 1, 128
    IF (ICHAR(CC(I8:I8), KIND=ICHAR(CHAR(8, KIND=SIZE((/T%K1/))), KIND=T%K8))  .NE. I8-1 ) ERROR STOP 81
    IF (ICHAR(CC(I8:I8), KIND=ICHAR(CHAR(8, KIND=SIZE((/T%K2/))), KIND=T%K4))  .NE. I8-1 ) ERROR STOP 82
    IF (ICHAR(CC(I8:I8), KIND=ICHAR(CHAR(8, KIND=SIZE((/T%K4/))), KIND=T%K2))  .NE. I8-1 ) ERROR STOP 83
    IF (ICHAR(CC(I8:I8), KIND=ICHAR(CHAR(8, KIND=SIZE((/T%K8/))), KIND=T%K1))  .NE. I8-1 ) ERROR STOP 84
  END DO

  II1=(/(I1, I1=0,127)/)
  IF (ANY(ICHAR(C=(/(CC(I:I), I=1,128)/), KIND=CC%KIND)      .NE. II1 )) ERROR STOP 111
  IF (ANY(ICHAR(C=(/(CC(I:I), I=1,128)/), KIND=SIZE((/CC/))) .NE. II1 )) ERROR STOP 112


  END

