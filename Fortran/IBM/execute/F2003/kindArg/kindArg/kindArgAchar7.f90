!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgAchar7
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : ACHAR 
!*
!*  REFERENCE                  : Feature Number 289083 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!*   
!*  Entities with different attubute used for kind arg - function return 
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgAchar7


  INTEGER(1), ALLOCATABLE :: I1, II1(:), K1
  INTEGER(2), POINTER     :: I2, II2(:), K2
  INTEGER(4), ALLOCATABLE :: I4, II4(:), K4
  INTEGER(8), POINTER     :: I8, II8(:), K8
     
! CHARACTER :: CC=(/(ACHAR(I=I, KIND=1), I=0, 127)/)

  TYPE :: DT
    INTEGER(1) :: K1=0
    INTEGER(1) :: K2=0
    INTEGER(1) :: K4=0
    INTEGER(1) :: K8=0
  END TYPE

  TYPE (DT), PARAMETER :: T=DT(1,2,4,8)

  CHARACTER :: CC(0:127)
  CC=(/(ACHAR(I=I, KIND=1), I=0, 127)/)
 
  ALLOCATE(I1)
  ALLOCATE(I2)
  ALLOCATE(I4)
  ALLOCATE(I8)

  ALLOCATE(II1(128))
  ALLOCATE(II2(128))
  ALLOCATE(II4(128))
  ALLOCATE(II8(128))

  DO I1 = 0, 127
    IF (ANY(ACHAR(I1, KIND=KIND(IACHAR(ACHAR(I1, KIND=1_1), KIND=1_1)))  .NE. CC(I1:I1)) ) STOP 11
    IF (ANY(ACHAR(I1, KIND=KIND(IACHAR(ACHAR(I1, KIND=1_2), KIND=1_2)))  .NE. CC(I1:I1)) ) STOP 12
    IF (ANY(ACHAR(I1, KIND=KIND(IACHAR(ACHAR(I1, KIND=1_4), KIND=1_4)))  .NE. CC(I1:I1)) ) STOP 13
    IF (ANY(ACHAR(I1, KIND=KIND(IACHAR(ACHAR(I1, KIND=1_8), KIND=1_8)))  .NE. CC(I1:I1)) ) STOP 14
  END DO

  DO I2 = 0, 127
    IF (ANY(ACHAR(I2, KIND=IACHAR(ACHAR(1), KIND=T%K8))  .NE. CC(I2:I2)) ) STOP 21
    IF (ANY(ACHAR(I2, KIND=IACHAR(ACHAR(1), KIND=T%K4))  .NE. CC(I2:I2)) ) STOP 22
    IF (ANY(ACHAR(I2, KIND=IACHAR(ACHAR(1), KIND=T%K2))  .NE. CC(I2:I2)) ) STOP 23
    IF (ANY(ACHAR(I2, KIND=IACHAR(ACHAR(1), KIND=T%K1))  .NE. CC(I2:I2)) ) STOP 24
  END DO


  II1=(/(I1, I1=0,127)/)
  IF (ANY(ACHAR(I=II1, KIND=IACHAR(ACHAR(I=1, KIND=1_1), KIND=T%K1))  .NE. CC )) STOP 111
  IF (ANY(ACHAR(I=II1, KIND=IACHAR(ACHAR(I=1, KIND=1_2), KIND=T%K1))  .NE. CC )) STOP 112
  IF (ANY(ACHAR(I=II1, KIND=IACHAR(ACHAR(I=1, KIND=1_4), KIND=T%K1))  .NE. CC )) STOP 113
  IF (ANY(ACHAR(I=II1, KIND=IACHAR(ACHAR(I=1, KIND=1_8), KIND=T%K1))  .NE. CC )) STOP 114

  II2=(/(I2, I2=0,127)/)
  IF (ANY(ACHAR(I=II2, KIND=KIND(IACHAR(ACHAR(I=II2, KIND=T%K1), KIND=1_8)))  .NE. CC )) STOP 121
  IF (ANY(ACHAR(I=II2, KIND=KIND(IACHAR(ACHAR(I=II2, KIND=T%K1), KIND=1_4)))  .NE. CC )) STOP 122
  IF (ANY(ACHAR(I=II2, KIND=KIND(IACHAR(ACHAR(I=II2, KIND=T%K1), KIND=1_2)))  .NE. CC )) STOP 123
  IF (ANY(ACHAR(I=II2, KIND=KIND(IACHAR(ACHAR(I=II2, KIND=T%K1), KIND=1_1)))  .NE. CC )) STOP 124



  END

