! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrArrSec.f
! opt variations: -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 15, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  the array section
!*
!*  (323291)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrArrSec
  IMPLICIT NONE

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  END TYPE

  CLASS(DT(:,4)), POINTER :: Arr(:, :), Arr1(:)
  CLASS(DT(:,4)), POINTER :: Ptr(:, :)
  INTEGER            :: I, J, N, K

  N = 100
  ALLOCATE(Arr(N,N), SOURCE=DT(20,4)(-1))
  ALLOCATE(Arr1(N*N), SOURCE=DT(20,4)(-2))

  Arr%ID  = RESHAPE((/(i, i=1, 10000)/), (/100, 100/))
  Arr1%ID = (/(i, i=1, 10000)/)

  DO I =1, N
  DO J =I, N

    Ptr => Arr
    Ptr(I:, J:) => Ptr(N:I:-1, N:J:-1)
    IF (.NOT. ASSOCIATED(Ptr,  Arr(N:I:-1, N:J:-1) ))     ERROR STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I , J/)))       ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/N,  N/)))       ERROR STOP 13
    IF (ANY( Ptr%ID      .NE. Arr(N:I:-1, N:I:-1)%ID ))    ERROR STOP 14

    Ptr(I:J, I:J) => Arr1(N*N:N*N-(J-I+1)*(J-I+1)+1:-1)
    IF (.NOT. ASSOCIATED(Ptr))                   ERROR STOP 20
    IF (SIZE(Ptr)         .NE. (J-I+1)*(J-I+1))  ERROR STOP 21
    IF (ANY( LBOUND(Ptr)  .NE. (/I , I /)))      ERROR STOP 22
    IF (ANY( UBOUND(Ptr)  .NE. (/J , J /)))      ERROR STOP 23
    IF (ANY( RESHAPE(Ptr%ID, (/(J-I+1)*(J-I+1)/)) .NE.  &
    &  (/(K, K=N*N,N*N-(J-I+1)*(J-I+1), -1)/) ))   ERROR STOP 24

  END DO
  END DO


  END


