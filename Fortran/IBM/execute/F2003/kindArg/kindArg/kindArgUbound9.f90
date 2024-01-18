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
!*  -qintsize
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgUbound9
  IMPLICIT NONE


  TYPE :: DT1
    CLASS(*), POINTER :: Arr(:,:,:,:,:,:,:,:,:)
    CLASS(*), POINTER :: Arr0(:,:,:,:,:,:,:,:,:)
  END TYPE

  TYPE(DT1)          :: TT
  INTEGER            :: I
  INTEGER, PARAMETER :: L2= -127, L1= -128

  integer :: ii(2:1,2:1)

  ALLOCATE(  TT%Arr(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2), SOURCE=1)

  DO I = 1, 9
    IF (     UBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(I))   .NE. L2)               STOP 11
    IF (KIND(UBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(I)))  .NE. KIND(I))          STOP 12
  END DO

  IF (ANY (UBOUND(ARRAY=TT%Arr, KIND=KIND(I))      .NE. L2 ) )       STOP 21
  IF (KIND(UBOUND(ARRAY=TT%Arr, KIND=KIND(I)))     .NE. KIND(I) )    STOP 22


  ALLOCATE( TT%Arr0(L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1), SOURCE=(0.,0.))

  DO I = 1, 9
    IF (     UBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(I))   .NE.  0)               STOP 31
    IF (KIND(UBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(I)))  .NE. KIND(I))          STOP 32
  END DO

  IF (ANY (UBOUND(ARRAY=TT%Arr0, KIND=KIND(I))      .NE.  0 ) )       STOP 41
  IF (KIND(UBOUND(ARRAY=TT%Arr0, KIND=KIND(I)))     .NE. KIND(I) )    STOP 42


  END

