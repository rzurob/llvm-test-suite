!*********************************************************************
!*  ===================================================================
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

  TYPE :: DT
    INTEGER            :: ID=0
    CLASS(DT), POINTER :: Ptr(:)
  END TYPE

  TYPE, EXTENDS(DT) :: DT1
    INTEGER, PRIVATE :: ID1=1
  END TYPE

  END MODULE


  PROGRAM dataPtrDeallocate
  USE M
  IMPLICIT NONE

  TYPE(DT)   :: T(100)
  INTEGER    :: I, J, K, N

  N = 100; K = 0

  DO I=1, N
    ALLOCATE(T(I)%Ptr(I), SOURCE=DT1(ID=I, Ptr=NULL()))
  END DO

  DO I =1, N
    T(I)%Ptr(I:) => T(I)%Ptr
    IF (.NOT. ASSOCIATED(T(I)%Ptr))                 ERROR STOP 11
    IF (ANY( LBOUND(T(I)%Ptr) .NE. (/I /)))         ERROR STOP 12
    IF (ANY( UBOUND(T(I)%Ptr) .NE. (/2*I-1/)))      ERROR STOP 13
    IF (ANY( T(I)%Ptr%ID      .NE.  I ))            ERROR STOP 14
  END DO

  DO I =1, N
  DO J =I, 1, -1
    T(I)%Ptr(I:I+J-1) => T(I)%Ptr
    IF (.NOT. ASSOCIATED(T(I)%Ptr))                 ERROR STOP 21
    IF (ANY( LBOUND(T(I)%Ptr) .NE. (/I /)))         ERROR STOP 22
    IF (ANY( UBOUND(T(I)%Ptr) .NE. (/I+J-1/)))      ERROR STOP 23
    IF (ANY( T(I)%Ptr%ID      .NE.  I ))            ERROR STOP 24
  END DO
  END DO

  DO I=2, N  ! Exclude the deallocation of T(1)%Ptr
    DEALLOCATE(T(I)%Ptr, STAT=K)
    IF ( K .EQ. 0) ERROR STOP 55
  END DO

  END

