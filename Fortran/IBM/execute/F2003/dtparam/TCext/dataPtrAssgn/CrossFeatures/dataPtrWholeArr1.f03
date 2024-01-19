! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrWholeArr1.f
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
!*  the whole array
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    SEQUENCE
    INTEGER(K1)   :: ID
  END TYPE

  TYPE(DT(:,4)),  POINTER :: Ptr(:, :)

  END MODULE


  PROGRAM dataPtrWholeArr1
  USE M, ONLY :  Ptr
  IMPLICIT NONE

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    SEQUENCE
    INTEGER(K1)   :: ID
  END TYPE

  TYPE(DT(20,4)),  TARGET  :: Arr(100, 100), Arr1(10000)
  INTEGER            :: I, J, N

  N = 100
  Arr  = DT(20,4)(-1)
  Arr1 = DT(20,4)(-2)


  DO I =1, N
  DO J =I, N

    Ptr(I:, J:) => Arr
    IF (.NOT. ASSOCIATED(Ptr,  Arr ))           ERROR STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I , J/)))       ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1,J+N-1/))) ERROR STOP 13
    IF (ANY( Ptr%ID .NE. -1 ))                  ERROR STOP 14

    Ptr(I:J, I:J) => Arr1
    IF (.NOT. ASSOCIATED(Ptr))                   ERROR STOP 20
    IF (SIZE(Ptr)         .NE. (J-I+1)*(J-I+1))  ERROR STOP 21
    IF (ANY( LBOUND(Ptr)  .NE. (/I , I /)))      ERROR STOP 22
    IF (ANY( UBOUND(Ptr)  .NE. (/J , J /)))      ERROR STOP 23
    IF (ANY( Ptr%ID .NE. -2 ))                   ERROR STOP 24

  END DO
  END DO


  END


