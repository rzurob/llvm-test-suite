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

  TYPE :: DT
    INTEGER :: ID
  END TYPE

  CLASS(DT), POINTER :: Arr(:, :), Arr1(:)
  CLASS(DT), POINTER :: Ptr(:, :)
  INTEGER            :: I, J, N, K

  N = 100
  ALLOCATE(Arr(N,N), SOURCE=DT(-1))
  ALLOCATE(Arr1(N*N), SOURCE=DT(-2))

  Arr%ID  = RESHAPE((/(i, i=1, 10000)/), (/100, 100/))
  Arr1%ID = (/(i, i=1, 10000)/)

  DO I =1, N
  DO J =I, N

    Ptr => Arr
    Ptr(I:, J:) => Ptr(N:I:-1, N:J:-1)
    IF (.NOT. ASSOCIATED(Ptr,  Arr(N:I:-1, N:J:-1) ))     STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I , J/)))       STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/N,  N/)))       STOP 13
    IF (ANY( Ptr%ID      .NE. Arr(N:I:-1, N:I:-1)%ID ))    STOP 14

    Ptr(I:J, I:J) => Arr1(N*N:N*N-(J-I+1)*(J-I+1)+1:-1)
    IF (.NOT. ASSOCIATED(Ptr))                   STOP 20
    IF (SIZE(Ptr)         .NE. (J-I+1)*(J-I+1))  STOP 21
    IF (ANY( LBOUND(Ptr)  .NE. (/I , I /)))      STOP 22
    IF (ANY( UBOUND(Ptr)  .NE. (/J , J /)))      STOP 23
    IF (ANY( RESHAPE(Ptr%ID, (/(J-I+1)*(J-I+1)/)) .NE.  &
    &  (/(K, K=N*N,N*N-(J-I+1)*(J-I+1), -1)/) ))   STOP 24

  END DO
  END DO


  END


