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
!*  substring
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrSubStr
  IMPLICIT NONE
  CHARACTER(513), TARGET  :: Arr(100, 100), Arr1(10000)
  CHARACTER               :: C

  C = CHAR(1)
  Arr = REPEAT(C, 513)
  Arr1 = REPEAT(C, 513)
  CALL Sub(Arr, Arr1, REPEAT(C, 513), 100)

  CONTAINS

  SUBROUTINE Sub(Arr, Arr1, Str, N)
  CHARACTER(*)                  :: Str
  CHARACTER(*), TARGET          :: Arr(N, N), Arr1(N*N)
  CHARACTER(LEN(Arr)), POINTER  :: Ptr(:, :)
  INTEGER                       :: I, J, N


  DO I =1, 100
  DO J =I, 100

    Ptr(I:, J:) => Arr(I:, J:)(1:LEN(Str))
    IF (.NOT. ASSOCIATED(Ptr, Arr(I:, J:) ))    ERROR STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I , J/)))       ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/N,  N/)))       ERROR STOP 13
    IF (ANY( Ptr(I:, J:) .NE. Str ))            ERROR STOP 14

    Ptr(I:J, I:J) => Arr1(1:N*N-(J-I+1)*(J-I+1))(1:LEN(Str))
    IF (.NOT. ASSOCIATED(Ptr))                  ERROR STOP 21
    IF (SIZE(Ptr) .NE. (J-I+1)*(J-I+1))         ERROR STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I , I /)))      ERROR STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J , J /)))      ERROR STOP 23
    IF (ANY( Ptr(I:J, I:J) .NE. Str ))          ERROR STOP 24

  END DO
  END DO

  END SUBROUTINE

  END


