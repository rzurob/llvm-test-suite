!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 22, 2006
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
!*  -qintsize
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLbound9
  IMPLICIT NONE

  TYPE :: DT
    SEQUENCE
    CHARACTER :: C
    LOGICAL(2):: L
    INTEGER   :: I
    REAL(8)   :: R
    COMPLEX   :: Z
  END TYPE

  TYPE :: DT1
    CLASS(*), POINTER :: Arr(:,:,:,:,:,:,:,:,:)
    CLASS(*), POINTER :: Arr0(:,:,:,:,:,:,:,:,:)
  END TYPE

  TYPE(DT1)          :: TT
  INTEGER            :: I
  INTEGER, PARAMETER :: L2= -127, L1= -128


  ALLOCATE( DT1 ::  TT%Arr(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2))

  DO I = 1, 9
    IF (     LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(I))   .NE. L1)               STOP 11
    IF (KIND(LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(I)))  .NE. KIND(I))          STOP 12
  END DO

  IF (ANY (LBOUND(ARRAY=TT%Arr, KIND=KIND(I))      .NE. L1 ) )       STOP 21
  IF (KIND(LBOUND(ARRAY=TT%Arr, KIND=KIND(I)))     .NE. KIND(I) )    STOP 22


  ALLOCATE( DT1 ::  TT%Arr0(L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1))

  DO I = 1, 9
    IF (     LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(I))   .NE.  1)               STOP 31
    IF (KIND(LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(I)))  .NE. KIND(I))          STOP 32
  END DO

  IF (ANY (LBOUND(ARRAY=TT%Arr0, KIND=KIND(I))      .NE.  1 ) )       STOP 41
  IF (KIND(LBOUND(ARRAY=TT%Arr0, KIND=KIND(I)))     .NE. KIND(I) )    STOP 42


  END

