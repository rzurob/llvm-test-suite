! GB DTP extension using:
! ftcx_dtp -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/kindArg/kindArg/kindArgUbound7.f
! opt variations: -qck -qdefaultpv -qnodeferredlp -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 22, 2006
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
!*  Case (i):
!*  For an array section or for an array expression, other than a whole array or array
!*  structure component, UBOUND (ARRAY, DIM) has a value equal to the number
!*  of elements in the given dimension; otherwise, it has a value equal to the upper
!*  bound for subscript DIM of ARRAY if dimension DIM of ARRAY does not have
!*  size zero and has the value zero if dimension DIM has size zero.
!*  -- zero extend
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLbound7
  IMPLICIT NONE

  INTEGER :: I, I1

  TYPE :: DT(N1,D1,D2,D3)    ! (1,2,4,8)
    INTEGER, KIND :: D1,D2,D3
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: C
    LOGICAL(D1)   :: L
    INTEGER(D2)   :: I
    REAL(D3)      :: R
    COMPLEX(D2)   :: Z
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (1,2,4,8)
    CLASS(DT(:,D1,D2,D3)), POINTER :: Arr(:,:,:,:,:,:,:,:,:)
    CLASS(DT(:,D1,D2,D3)), POINTER :: Arr0(:,:,:,:,:,:,:,:,:)
  END TYPE

  TYPE(DT1(1,2,4,8))          :: TT
  TYPE(DT(1,2,4,8))           :: T
  INTEGER, PARAMETER :: L2= 127, L1= -128


  !ALLOCATE( DT1 ::  TT%Arr(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2))
  ALLOCATE( DT1(1,2,4,8) ::  TT%Arr0(L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1))


  DO I = 1, 9
    IF (     UBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%C))   .NE. 0 )               ERROR STOP 11
    IF (KIND(UBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%C)))  .NE. 1)                ERROR STOP 12
    IF (     UBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%L))   .NE. 0 )               ERROR STOP 13
    IF (KIND(UBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%L)))  .NE. 2)                ERROR STOP 14
    IF (     UBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%I))   .NE. 0 )               ERROR STOP 15
    IF (KIND(UBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%I)))  .NE. 4)                ERROR STOP 16
    IF (     UBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%R))   .NE. 0 )               ERROR STOP 17
    IF (KIND(UBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%R)))  .NE. 8)                ERROR STOP 18
  END DO

  IF (SIZE(UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%C)))  .NE. 9)                ERROR STOP 20
  IF (ANY( UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%C))   .NE. 0))               ERROR STOP 21
  IF (KIND(UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%C)))  .NE. 1)                ERROR STOP 22

  IF (SIZE(UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%L)))  .NE. 9)                ERROR STOP 30
  IF (ANY( UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%L))   .NE. 0 ))              ERROR STOP 31
  IF (KIND(UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%L)))  .NE. 2)                ERROR STOP 32

  IF (SIZE(UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%I)))  .NE. 9)                ERROR STOP 40
  IF (ANY( UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%I))   .NE. 0 ))              ERROR STOP 41
  IF (KIND(UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%I)))  .NE. 4)                ERROR STOP 42

  IF (SIZE(UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%R)))  .NE. 9)                ERROR STOP 50
  IF (ANY( UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%R))   .NE. 0 ))              ERROR STOP 51
  IF (KIND(UBOUND(ARRAY=TT%Arr0, KIND=KIND(T%R)))  .NE. 8)                ERROR STOP 52

  IF (ANY( UBOUND(ARRAY=TT%Arr0)   .NE. 0 ))                      ERROR STOP 31
  IF (KIND(UBOUND(ARRAY=TT%Arr0))  .NE. 4)                        ERROR STOP 32

  END

