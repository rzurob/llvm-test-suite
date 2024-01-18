! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=self /tstdev/F2003/kindArg/kindArg/kindArgLbound6.f
! opt variations: -qck -qnok -ql -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLbound6
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
!*  -- array component
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLbound6
  IMPLICIT NONE

  INTEGER :: I, I1

  TYPE :: DT(N1,D1,D2,D3)    ! (1,2,4,8)
    INTEGER, KIND :: D1,D2,D3
    INTEGER, LEN  :: N1
    SEQUENCE
    CHARACTER(N1) :: C
    LOGICAL(D1)   :: L
    INTEGER(D2)   :: I
    REAL(D3)      :: R
    COMPLEX(D2)   :: Z
  END TYPE

  TYPE :: DT1(D4)    ! (4)
      INTEGER, KIND :: D4
    CLASS(*), POINTER :: Arr(:,:,:,:,:,:,:,:,:)
    CLASS(*), POINTER :: Arr0(:,:,:,:,:,:,:,:,:)
  END TYPE

  TYPE(DT1(4))          :: TT
  TYPE(DT(1,2,4,8))           :: T
  INTEGER, PARAMETER :: L2= 127, L1= -128


  ALLOCATE( DT1(4) ::  TT%Arr(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2))
  ALLOCATE( DT1(4) ::  TT%Arr0(L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1))


  DO I = 1, 9
    IF (     LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(T%C))   .NE. L1)               STOP 11
    IF (KIND(LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(T%C)))  .NE. 1)                STOP 12
    IF (     LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(T%L))   .NE. L1)               STOP 13
    IF (KIND(LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(T%L)))  .NE. 2)                STOP 14
    IF (     LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(T%I))   .NE. L1)               STOP 15
    IF (KIND(LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(T%I)))  .NE. 4)                STOP 16
    IF (     LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(T%R))   .NE. L1)               STOP 17
    IF (KIND(LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(T%R)))  .NE. 8)                STOP 18
  END DO


  IF (SIZE(LBOUND(ARRAY=TT%Arr, KIND=KIND(T%C)))  .NE. 9)                STOP 20
  IF (ANY( LBOUND(ARRAY=TT%Arr, KIND=KIND(T%C))   .NE. L1))              STOP 21
  IF (KIND(LBOUND(ARRAY=TT%Arr, KIND=KIND(T%C)))  .NE. 1)                STOP 22

  IF (SIZE(LBOUND(ARRAY=TT%Arr, KIND=KIND(T%L)))  .NE. 9)                STOP 30
  IF (ANY( LBOUND(ARRAY=TT%Arr, KIND=KIND(T%L))   .NE. L1))              STOP 31
  IF (KIND(LBOUND(ARRAY=TT%Arr, KIND=KIND(T%L)))  .NE. 2)                STOP 32

  IF (SIZE(LBOUND(ARRAY=TT%Arr, KIND=KIND(T%I)))  .NE. 9)                STOP 40
  IF (ANY( LBOUND(ARRAY=TT%Arr, KIND=KIND(T%I))   .NE. L1))              STOP 41
  IF (KIND(LBOUND(ARRAY=TT%Arr, KIND=KIND(T%I)))  .NE. 4)                STOP 42

  IF (SIZE(LBOUND(ARRAY=TT%Arr, KIND=KIND(T%R)))  .NE. 9)                STOP 50
  IF (ANY( LBOUND(ARRAY=TT%Arr, KIND=KIND(T%R))   .NE. L1))              STOP 51
  IF (KIND(LBOUND(ARRAY=TT%Arr, KIND=KIND(T%R)))  .NE. 8)                STOP 52

  IF (ANY( LBOUND(ARRAY=TT%Arr)   .NE. L1))                      STOP 31
  IF (KIND(LBOUND(ARRAY=TT%Arr))  .NE. 4)                        STOP 32

  END

