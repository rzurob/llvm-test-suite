!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 10, 2006
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
!*  Assumed size array of character
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrAssumSiz3
  IMPLICIT NONE

  CHARACTER(2), TARGET :: Arr(10,10)
  CHARACTER(100), TARGET :: C
  INTEGER                :: I


  Arr = "12"
  CALL S(Arr(1, 1), 10, 0, 19)

  DO I=1, 100, 2
    C(I:I) ="1"
    C(I+1:I+1) ="2"
  END DO
  CALL S1(C(1:), 10, 0, 9)
  CALL S1(C, 10, 0, 9)

  CONTAINS

  SUBROUTINE S(Arr, N, L, U)
  CHARACTER(1), TARGET  :: Arr(L:U, *)
  INTEGER               :: N, L, U
  CHARACTER(1), POINTER :: Ptr(:, :)


  Ptr(L:, L:) => Arr(:,1:10)

  IF (.NOT. ASSOCIATED(Ptr, Arr(:, 1:10)))          STOP 11
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L  /)))    STOP 12
  IF (ANY( UBOUND(Ptr)         .NE. (/U, L+9/)))    STOP 13
  IF (ANY( Ptr(L::2, :)        .NE.   "1"))         STOP 14
  IF (ANY( Ptr(L+1::2,:)       .NE.   "2"))         STOP 15

  Ptr(L:U, L:L) => Arr(:,1)

  IF (.NOT. ASSOCIATED(Ptr))                       STOP 21
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L /)))    STOP 22
  IF (ANY( UBOUND(Ptr)         .NE. (/U, L /)))    STOP 23
  IF (ANY( Ptr(L::2, :)        .NE.   "1"))        STOP 24
  IF (ANY( Ptr(L+1::2,:)       .NE.   "2"))        STOP 25

  END SUBROUTINE

  SUBROUTINE S1(Arr, N, L, U)
  CHARACTER(1), TARGET  :: Arr(L:U, *)
  INTEGER               :: N, L, U
  CHARACTER(1), POINTER :: Ptr(:, :)


  Ptr(L:, L:) => Arr(:,1:10)

  IF (.NOT. ASSOCIATED(Ptr, Arr(:, 1:10)))          STOP 31
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L  /)))    STOP 32
  IF (ANY( UBOUND(Ptr)         .NE. (/U, L+9/)))    STOP 33
  IF (ANY( Ptr(L::2, :)        .NE.   "1"))         STOP 34
  IF (ANY( Ptr(L+1::2,:)       .NE.   "2"))         STOP 35

  Ptr(L:U, L:L) => Arr(:,1)

  IF (.NOT. ASSOCIATED(Ptr))                       STOP 41
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L /)))    STOP 42
  IF (ANY( UBOUND(Ptr)         .NE. (/U, L /)))    STOP 43
  IF (ANY( Ptr(L::2, :)        .NE.   "1"))        STOP 44
  IF (ANY( Ptr(L+1::2,:)       .NE.   "2"))        STOP 45

  END SUBROUTINE

  END



