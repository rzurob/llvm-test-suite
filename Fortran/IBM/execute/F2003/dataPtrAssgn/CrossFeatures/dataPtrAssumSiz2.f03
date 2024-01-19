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
!*  Assumed size array
!*  Actual arg is an element.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT
    INTEGER    :: ID
  CONTAINS
    PROCEDURE ModFun
  END TYPE

  CONTAINS

  ELEMENTAL FUNCTION ModFun(Arg)
  CLASS(DT), INTENT(IN) :: Arg
  INTEGER   :: ModFun
    ModFun = Arg%ID
  END FUNCTION

  END MODULE


  PROGRAM dataPtrAssumSiz2
  USE M
  IMPLICIT NONE

  TYPE(DT),     TARGET :: Arr(10,10)

  Arr = DT(-1)

  CALL S(Arr(1, 1), 10, 0, 9)

  CONTAINS

  SUBROUTINE S(Arr, N, L, U)
  CLASS(DT), TARGET  :: Arr(L:L, U:*)
  INTEGER            :: N, L, U
  CLASS(DT), POINTER :: Ptr(:, :)


  Ptr(L:, L:) => Arr(:,U:U)

  IF (.NOT. ASSOCIATED(Ptr, Arr(:, U:U)))          ERROR STOP 11
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L /)))    ERROR STOP 12
  IF (ANY( UBOUND(Ptr)         .NE. (/L, L /)))    ERROR STOP 13
  IF (ANY( Ptr%ID              .NE.   -1))         ERROR STOP 14
  Ptr%ID = 1
  IF (ANY( Ptr%ModFun()        .NE.    1))         ERROR STOP 15

  Ptr(L:L, U:U) => Arr(:,U)

  IF (.NOT. ASSOCIATED(Ptr))                       ERROR STOP 21
  IF (ANY( LBOUND(Ptr)         .NE. (/L, U /)))    ERROR STOP 22
  IF (ANY( UBOUND(Ptr)         .NE. (/L, U /)))    ERROR STOP 23
  IF (ANY( Ptr%ID              .NE.    1))         ERROR STOP 24
  Ptr%ID = -1
  IF (ANY( Ptr%ModFun()        .NE.   -1))         ERROR STOP 25

  END SUBROUTINE

  END



