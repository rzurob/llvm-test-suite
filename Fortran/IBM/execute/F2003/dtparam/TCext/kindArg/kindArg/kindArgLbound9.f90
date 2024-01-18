! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/F2003/kindArg/kindArg/kindArgLbound9.f
! opt variations: -qnock -qnok -qnol -qreuse=self

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

  TYPE :: DT(D1,N1,D2,D3,D4,D5)    ! (1,1,2,4,8,4)
    INTEGER, KIND             :: D1,D2,D3,D4,D5
    INTEGER, LEN              :: N1
    SEQUENCE
    CHARACTER(kind=D1,len=N1) :: C
    LOGICAL(D2)               :: L
    INTEGER(D3)               :: I
    REAL(D4)                  :: R
    COMPLEX(D5)               :: Z
  END TYPE

  TYPE :: DT1(D6,N2)    ! (4,20)
    INTEGER, KIND :: D6
    INTEGER, LEN  :: N2
    CLASS(*), POINTER :: Arr(:,:,:,:,:,:,:,:,:)
    CLASS(*), POINTER :: Arr0(:,:,:,:,:,:,:,:,:)
  END TYPE

  TYPE(DT1(4,20))          :: TT
  INTEGER            :: I
  INTEGER, PARAMETER :: L2= -127, L1= -128


  ALLOCATE( DT1(4,20) ::  TT%Arr(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2))

  DO I = 1, 9
    IF (     LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(I))   .NE. L1)               ERROR STOP 11
    IF (KIND(LBOUND(ARRAY=TT%Arr, DIM=I, KIND=KIND(I)))  .NE. KIND(I))          ERROR STOP 12
  END DO

  IF (ANY (LBOUND(ARRAY=TT%Arr, KIND=KIND(I))      .NE. L1 ) )       ERROR STOP 21
  IF (KIND(LBOUND(ARRAY=TT%Arr, KIND=KIND(I)))     .NE. KIND(I) )    ERROR STOP 22


  ALLOCATE( DT1(4,20) ::  TT%Arr0(L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1,L2:L1))

  DO I = 1, 9
    IF (     LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(I))   .NE.  1)               ERROR STOP 31
    IF (KIND(LBOUND(ARRAY=TT%Arr0, DIM=I, KIND=KIND(I)))  .NE. KIND(I))          ERROR STOP 32
  END DO

  IF (ANY (LBOUND(ARRAY=TT%Arr0, KIND=KIND(I))      .NE.  1 ) )       ERROR STOP 41
  IF (KIND(LBOUND(ARRAY=TT%Arr0, KIND=KIND(I)))     .NE. KIND(I) )    ERROR STOP 42


  END

