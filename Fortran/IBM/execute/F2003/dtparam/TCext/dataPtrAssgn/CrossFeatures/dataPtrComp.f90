! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrComp.f
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
!*  Component
!*
!*  (323380)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrComp
  IMPLICIT NONE

  TYPE :: DT0(N1,K1)    ! (20,4)
    INTEGER, KIND        :: K1
    INTEGER, LEN         :: N1
    INTEGER(K1), POINTER :: PArr0(:,:), PArr1(:)
  END TYPE

  TYPE, EXTENDS(DT0) :: DT    ! (20,4)
  END TYPE

  TYPE(DT(20,4)), TARGET :: T(10, 10), T1(100)
  INTEGER, TARGET  :: TarArr(10, 10), TarArr1(100)

  TarArr  = -1
  TarArr1 = -2

  T  = DT(20,4)(TarArr, TarArr1)
  T1 = DT(20,4)(TarArr, TarArr1)

  CALL Sub(T, T1, 10)

  CONTAINS

  SUBROUTINE Sub(Arr, Arr1, N)
  TYPE(DT(*,4)), TARGET   :: Arr(N, N), Arr1(N*N)
  CLASS(DT(:,4)), POINTER :: Ptr(:, :)
  INTEGER            :: I, J, N

  DO I =1, N
  DO J =I, N

    Ptr(I:, J:) => Arr(I:, J:)
    Ptr(I, J)%PArr0(I:, J:) => Ptr(I, J)%PArr0(I:, J:)
    IF (.NOT. ASSOCIATED(Ptr(I, J)%PArr0,  Arr(I, J)%PArr0(I:, J:) ))    ERROR STOP 11
    IF (ANY( LBOUND(Ptr(I, J)%PArr0) .NE. (/I , J/)))                    ERROR STOP 12
    IF (ANY( UBOUND(Ptr(I, J)%PArr0) .NE. (/N,  N/)))                    ERROR STOP 13
    IF (ANY( Ptr(I, J)%PArr0 .NE. -1 ))                                  ERROR STOP 14

    Ptr(I:J, I:J) => Arr1(1:N*N-(J-I+1)*(J-I+1))
    Ptr(I, J)%PArr0(I:J, I:J) => Ptr(I, J)%PArr1(1:N*N-(J-I+1)*(J-I+1))
    IF (.NOT. ASSOCIATED(Ptr(I, J)%PArr0))                ERROR STOP 21
    IF (SIZE(Ptr(I, J)%PArr0) .NE. (J-I+1)*(J-I+1))       ERROR STOP 22
    IF (ANY( LBOUND(Ptr(I, J)%PArr0) .NE. (/I , I /)))    ERROR STOP 23
    IF (ANY( UBOUND(Ptr(I, J)%PArr0) .NE. (/J , J /)))    ERROR STOP 24
    IF (ANY( Ptr(I, J)%PArr0 .NE. -2 ))                   ERROR STOP 25

  END DO
  END DO

  END SUBROUTINE

  END


