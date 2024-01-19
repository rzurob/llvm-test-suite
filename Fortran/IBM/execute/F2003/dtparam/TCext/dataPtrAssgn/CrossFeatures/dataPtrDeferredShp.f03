! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrDeferredShp.f
! opt variations: -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 09, 2006
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
!*  Deferred shape array
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrDeferredShp
  IMPLICIT NONE

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (20,4)
  END TYPE

  CLASS(DT1(:,4)),  POINTER :: Arr(:, :, :)
  CLASS(DT1(:,4)),  ALLOCATABLE, TARGET :: Arr1(:, :, :)
  CLASS(DT(:,4)),   POINTER :: Ptr(:, :)
  INTEGER              :: L, U, N

  N =10
  L = 0
  U = 9

  ALLOCATE (Arr(L:U,L:U,L:U), SOURCE=DT1(20,4)(-1))

  Ptr(L:, L:) => Arr(L:,L:,L)%DT

  IF ( .NOT. ASSOCIATED(Ptr, Arr(L:,L:,L)))         ERROR STOP 11
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L /)))     ERROR STOP 12
  IF (ANY( UBOUND(Ptr)         .NE. (/U, U /)))     ERROR STOP 13
  IF (ANY( Ptr%ID              .NE. -1))            ERROR STOP 14

  Ptr(L:U, U:U) => Arr(L,L,:)%DT
  IF ( .NOT. ASSOCIATED(Ptr))                       ERROR STOP 21
  IF (ANY( LBOUND(Ptr)         .NE. (/L, U /)))     ERROR STOP 22
  IF (ANY( UBOUND(Ptr)         .NE. (/U, U /)))     ERROR STOP 23
  IF (ANY( Ptr%ID              .NE. -1))            ERROR STOP 24

  DEALLOCATE(Arr)
  ALLOCATE (Arr1(L:U,L:U,L:U), SOURCE=DT1(20,4)(-1))

  Ptr(L:, L:) => Arr1(L:,L:,L)

  IF ( .NOT. ASSOCIATED(Ptr, Arr1(L:,L:,L)))        ERROR STOP 31
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L /)))     ERROR STOP 32
  IF (ANY( UBOUND(Ptr)         .NE. (/U, U /)))     ERROR STOP 33
  IF (ANY( Ptr%ID              .NE. -1))            ERROR STOP 34

  Ptr(L:U, U:U) => Arr1(L,L,:)
  IF ( .NOT. ASSOCIATED(Ptr))                       ERROR STOP 41
  IF (ANY( LBOUND(Ptr)         .NE. (/L, U /)))     ERROR STOP 42
  IF (ANY( UBOUND(Ptr)         .NE. (/U, U /)))     ERROR STOP 43
  IF (ANY( Ptr%ID              .NE. -1))            ERROR STOP 44

  DEALLOCATE(Arr1)

  END


