!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgUbound6
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
!*  Case (i):
!*  For an array section or for an array expression, other than a whole array or array
!*  structure component, UBOUND (ARRAY, DIM) has a value equal to the number
!*  of elements in the given dimension; otherwise, it has a value equal to the upper
!*  bound for subscript DIM of ARRAY if dimension DIM of ARRAY does not have
!*  size zero and has the value zero if dimension DIM has size zero.
!*
!*  -- Array Section /Expr
!*  (322397)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgUbound6
  IMPLICIT NONE

  INTEGER :: I, I1

  TYPE :: DT
    CHARACTER :: C
    LOGICAL(2):: L
    INTEGER   :: I
    REAL(8)   :: R
    COMPLEX   :: Z
  END TYPE

  TYPE :: DT1
    CLASS(DT), POINTER :: Arr(:,:,:,:,:,:,:,:,:)
    CLASS(DT), POINTER :: Arr0(:,:,:,:,:,:,:,:,:)
  END TYPE

  TYPE(DT1)          :: TT
  TYPE(DT)           :: T
  INTEGER, PARAMETER :: L2= 0, L1= -1


  ALLOCATE( DT ::  TT%Arr(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2))
  ALLOCATE( DT ::  TT%Arr0(L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1))


  DO I = 1, 9
    IF (     UBOUND(ARRAY=TT%Arr%C, DIM=I, KIND=KIND(T%C))   .NE. 2)              STOP 11
    IF (KIND(UBOUND(ARRAY=TT%Arr%C, DIM=I, KIND=KIND(T%C)))  .NE. 1)              STOP 12
    IF (     UBOUND(ARRAY=TT%Arr%L, DIM=I, KIND=KIND(T%L))   .NE. 2)              STOP 13
    IF (KIND(UBOUND(ARRAY=TT%Arr%L, DIM=I, KIND=KIND(T%L)))  .NE. 2)              STOP 14
    IF (     UBOUND(ARRAY=TT%Arr%I, DIM=I, KIND=KIND(T%I))   .NE. 2)              STOP 15
    IF (KIND(UBOUND(ARRAY=TT%Arr%I, DIM=I, KIND=KIND(T%I)))  .NE. 4)              STOP 16
    IF (     UBOUND(ARRAY=TT%Arr%R, DIM=I, KIND=KIND(T%R))   .NE. 2)              STOP 17
    IF (KIND(UBOUND(ARRAY=TT%Arr%R, DIM=I, KIND=KIND(T%R)))  .NE. 8)              STOP 18
  END DO


  IF (SIZE(UBOUND(ARRAY=TT%Arr%C, KIND=KIND(T%C)))  .NE. 9)                STOP 20
  IF (ANY( UBOUND(ARRAY=TT%Arr%C, KIND=KIND(T%C))   .NE. 2))               STOP 21
  IF (KIND(UBOUND(ARRAY=TT%Arr%C, KIND=KIND(T%C)))  .NE. 1)                STOP 22

  IF (SIZE(UBOUND(ARRAY=TT%Arr%L, KIND=KIND(T%L)))  .NE. 9)                STOP 30
  IF (ANY( UBOUND(ARRAY=TT%Arr%L, KIND=KIND(T%L))   .NE. 2))               STOP 31
  IF (KIND(UBOUND(ARRAY=TT%Arr%L, KIND=KIND(T%L)))  .NE. 2)                STOP 32

  IF (SIZE(UBOUND(ARRAY=TT%Arr%I, KIND=KIND(T%I)))  .NE. 9)                STOP 40
  IF (ANY( UBOUND(ARRAY=TT%Arr%I, KIND=KIND(T%I))   .NE. 2))               STOP 41
  IF (KIND(UBOUND(ARRAY=TT%Arr%I, KIND=KIND(T%I)))  .NE. 4)                STOP 42

  IF (SIZE(UBOUND(ARRAY=TT%Arr%R, KIND=KIND(T%R)))  .NE. 9)                STOP 50
  IF (ANY( UBOUND(ARRAY=TT%Arr%R, KIND=KIND(T%R))   .NE. 2))               STOP 51
  IF (KIND(UBOUND(ARRAY=TT%Arr%R, KIND=KIND(T%R)))  .NE. 8)                STOP 52

  IF (ANY( UBOUND(ARRAY=TT%Arr%Z)   .NE. 2))                       STOP 31
  IF (KIND(UBOUND(ARRAY=TT%Arr%Z))  .NE. 4)                        STOP 32

  END

