! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=self /tstdev/F2003/kindArg/kindArg/kindArgLbound7.f
! opt variations: -qnock -qnok -qnol -qreuse=none

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
!*  Case (i): If ARRAY is a whole array or array structure component and either ARRAY is an
!*  assumed-size array of rank DIM or dimension DIM of ARRAY has nonzero extent,
!*  LBOUND (ARRAY, DIM) has a value equal to the lower bound for subscript DIM
!*  of ARRAY. Otherwise the result value is 1
!*  -- zero extend
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLbound7
  IMPLICIT NONE

  INTEGER :: I, I1

  TYPE :: DT(D1,N1,D2,D3,D4)    ! (1,1,2,4,8)
    INTEGER, KIND             :: D1,D2,D3,D4
    INTEGER, LEN              :: N1
    SEQUENCE
    CHARACTER(kind=D1,len=N1) :: C
    LOGICAL(D2)               :: L
    INTEGER(D3)               :: I
    REAL(D4)                  :: R
    COMPLEX(D3)               :: Z
  END TYPE

  TYPE :: DT1(D5,N2)    ! (4,20)
    INTEGER, KIND :: D5
    INTEGER, LEN  :: N2
    CLASS(*), POINTER :: Arr(:,:,:,:,:,:,:,:,:)
    CLASS(*), POINTER :: Arr0(:,:,:,:,:,:,:,:,:)
  END TYPE

  TYPE(DT1(4,20))          :: TT
  TYPE(DT(1,1,2,4,8))           :: T
  INTEGER, PARAMETER :: L2= 127, L1= -128


  ALLOCATE( DT1(4,20) ::  TT%Arr(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2))
  ALLOCATE( DT1(4,20) ::  TT%Arr0(L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1))


  DO I = 1, 9
    IF (     LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%C))   .NE. 1 )               ERROR STOP 11
    IF (KIND(LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%C)))  .NE. 1)                ERROR STOP 12
    IF (     LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%L))   .NE. 1 )               ERROR STOP 13
    IF (KIND(LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%L)))  .NE. 2)                ERROR STOP 14
    IF (     LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%I))   .NE. 1 )               ERROR STOP 15
    IF (KIND(LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%I)))  .NE. 4)                ERROR STOP 16
    IF (     LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%R))   .NE. 1 )               ERROR STOP 17
    IF (KIND(LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(T%R)))  .NE. 8)                ERROR STOP 18
  END DO


  IF (SIZE(LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%C)))  .NE. 9)                ERROR STOP 20
  IF (ANY( LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%C))   .NE. 1 ))              ERROR STOP 21
  IF (KIND(LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%C)))  .NE. 1)                ERROR STOP 22

  IF (SIZE(LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%L)))  .NE. 9)                ERROR STOP 30
  IF (ANY( LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%L))   .NE. 1 ))              ERROR STOP 31
  IF (KIND(LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%L)))  .NE. 2)                ERROR STOP 32

  IF (SIZE(LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%I)))  .NE. 9)                ERROR STOP 40
  IF (ANY( LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%I))   .NE. 1 ))              ERROR STOP 41
  IF (KIND(LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%I)))  .NE. 4)                ERROR STOP 42

  IF (SIZE(LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%R)))  .NE. 9)                ERROR STOP 50
  IF (ANY( LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%R))   .NE. 1 ))              ERROR STOP 51
  IF (KIND(LBOUND(ARRAY=TT%Arr0, KIND=KIND(T%R)))  .NE. 8)                ERROR STOP 52

  IF (ANY( LBOUND(ARRAY=TT%Arr0)   .NE. 1 ))                      ERROR STOP 31
  IF (KIND(LBOUND(ARRAY=TT%Arr0))  .NE. 4)                        ERROR STOP 32

  END
