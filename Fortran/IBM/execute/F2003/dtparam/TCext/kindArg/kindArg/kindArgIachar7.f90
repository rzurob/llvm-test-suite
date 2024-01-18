! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/kindArg/kindArg/kindArgIachar7.f
! opt variations: -qnol -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : IACHAR
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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIachar7


  INTEGER(1), ALLOCATABLE :: I1, II1(:), K1
  INTEGER(2), POINTER     :: I2, II2(:), K2
  INTEGER(4), ALLOCATABLE :: I4, II4(:), K4
  INTEGER(8), POINTER     :: I8, II8(:), K8

  CHARACTER :: CC(0:127)

  TYPE :: DT(N1,D1,D2,D3,D4)    ! (20,1,1,1,1)
    INTEGER, KIND :: D1,D2,D3,D4
    INTEGER, LEN  :: N1
    INTEGER(D1)   :: K1=0
    INTEGER(D2)   :: K2=0
    INTEGER(D3)   :: K4=0
    INTEGER(D4)   :: K8=0
  END TYPE

  TYPE (DT(20,1,1,1,1)), PARAMETER :: T=DT(20,1,1,1,1)(1,2,4,8)

  CC =(/(ACHAR(I=I, KIND=1), I=0, 127)/)

  ALLOCATE(I1)
  ALLOCATE(I2)
  ALLOCATE(I4)
  ALLOCATE(I8)

  ALLOCATE(II1(128))

  DO I1 = 0, 127
    IF (ANY(IACHAR(CC(I1:I1), KIND=IACHAR(ACHAR(1, KIND=SIZE((/T%K1/))), KIND=T%K8))  .NE. I1 )) STOP 11
    IF (ANY(IACHAR(CC(I1:I1), KIND=IACHAR(ACHAR(1, KIND=SIZE((/T%K2/))), KIND=T%K4))  .NE. I1 )) STOP 12
    IF (ANY(IACHAR(CC(I1:I1), KIND=IACHAR(ACHAR(1, KIND=SIZE((/T%K4/))), KIND=T%K2))  .NE. I1 )) STOP 13
    IF (ANY(IACHAR(CC(I1:I1), KIND=IACHAR(ACHAR(1, KIND=SIZE((/T%K8/))), KIND=T%K1))  .NE. I1 )) STOP 14
  END DO

  DO I2 = 0, 127
    IF (ANY(IACHAR(CC(I2:I2), KIND=IACHAR(ACHAR(2, KIND=SIZE((/T%K1/))), KIND=T%K8))  .NE. I2 )) STOP 21
    IF (ANY(IACHAR(CC(I2:I2), KIND=IACHAR(ACHAR(2, KIND=SIZE((/T%K2/))), KIND=T%K4))  .NE. I2 )) STOP 22
    IF (ANY(IACHAR(CC(I2:I2), KIND=IACHAR(ACHAR(2, KIND=SIZE((/T%K4/))), KIND=T%K2))  .NE. I2 )) STOP 23
    IF (ANY(IACHAR(CC(I2:I2), KIND=IACHAR(ACHAR(2, KIND=SIZE((/T%K8/))), KIND=T%K1))  .NE. I2 )) STOP 24
  END DO

  DO I4 = 0, 127
    IF (ANY(IACHAR(CC(I4:I4), KIND=IACHAR(ACHAR(4, KIND=SIZE((/T%K1/))), KIND=T%K8))  .NE. I4 )) STOP 41
    IF (ANY(IACHAR(CC(I4:I4), KIND=IACHAR(ACHAR(4, KIND=SIZE((/T%K2/))), KIND=T%K4))  .NE. I4 )) STOP 42
    IF (ANY(IACHAR(CC(I4:I4), KIND=IACHAR(ACHAR(4, KIND=SIZE((/T%K4/))), KIND=T%K2))  .NE. I4 )) STOP 43
    IF (ANY(IACHAR(CC(I4:I4), KIND=IACHAR(ACHAR(4, KIND=SIZE((/T%K8/))), KIND=T%K1))  .NE. I4 )) STOP 44
  END DO

  DO I8 = 0, 127
    IF (ANY(IACHAR(CC(I8:I8), KIND=IACHAR(ACHAR(8, KIND=SIZE((/T%K1/))), KIND=T%K8))  .NE. I8 )) STOP 81
    IF (ANY(IACHAR(CC(I8:I8), KIND=IACHAR(ACHAR(8, KIND=SIZE((/T%K2/))), KIND=T%K4))  .NE. I8 )) STOP 82
    IF (ANY(IACHAR(CC(I8:I8), KIND=IACHAR(ACHAR(8, KIND=SIZE((/T%K4/))), KIND=T%K2))  .NE. I8 )) STOP 83
    IF (ANY(IACHAR(CC(I8:I8), KIND=IACHAR(ACHAR(8, KIND=SIZE((/T%K8/))), KIND=T%K1))  .NE. I8 )) STOP 84
  END DO

  II1=(/(I1, I1=0,127)/)
  IF (ANY(IACHAR(C=CC(:), KIND=CC%KIND)  .NE. II1 )) STOP 111
  IF (ANY(IACHAR(C=CC(:), KIND=LEN(CC))  .NE. II1 )) STOP 112


  END

