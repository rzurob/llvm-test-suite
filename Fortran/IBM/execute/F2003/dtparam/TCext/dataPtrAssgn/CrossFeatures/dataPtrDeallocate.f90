! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrDeallocate.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrDeallocate.f
!*
!*  DATE                       : Feb. 16, 2006
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
!*  the deallocate stmt
!*
!*  (323635)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND            :: K1
    INTEGER, LEN             :: N1
    INTEGER(K1)              :: ID=0
    CLASS(DT(:,K1)), POINTER :: Ptr(:)
  END TYPE

  TYPE, EXTENDS(DT) :: DT1(N2,K2)    ! (20,4,20,4)
    INTEGER, KIND        :: K2
    INTEGER, LEN         :: N2
    INTEGER(K2), PRIVATE :: ID1=1
  END TYPE

  END MODULE


  PROGRAM dataPtrDeallocate
  USE M
  IMPLICIT NONE

  TYPE(DT(20,4))   :: T(100)
  INTEGER    :: I, J, K, N

  N = 100; K = 0

  DO I=1, N
    ALLOCATE(T(I)%Ptr(I), SOURCE=DT1(20,4,20,4)(ID=I, Ptr=NULL()))
  END DO

  DO I =1, N
    T(I)%Ptr(I:) => T(I)%Ptr
    IF (.NOT. ASSOCIATED(T(I)%Ptr))                 STOP 11
    IF (ANY( LBOUND(T(I)%Ptr) .NE. (/I /)))         STOP 12
    IF (ANY( UBOUND(T(I)%Ptr) .NE. (/2*I-1/)))      STOP 13
    IF (ANY( T(I)%Ptr%ID      .NE.  I ))            STOP 14
  END DO

  DO I =1, N
  DO J =I, 1, -1
    T(I)%Ptr(I:I+J-1) => T(I)%Ptr
    IF (.NOT. ASSOCIATED(T(I)%Ptr))                 STOP 21
    IF (ANY( LBOUND(T(I)%Ptr) .NE. (/I /)))         STOP 22
    IF (ANY( UBOUND(T(I)%Ptr) .NE. (/I+J-1/)))      STOP 23
    IF (ANY( T(I)%Ptr%ID      .NE.  I ))            STOP 24
  END DO
  END DO

  DO I=2, N  ! Exclude the deallocation of T(1)%Ptr
    DEALLOCATE(T(I)%Ptr, STAT=K)
    IF ( K .EQ. 0) STOP 55
  END DO

  END


