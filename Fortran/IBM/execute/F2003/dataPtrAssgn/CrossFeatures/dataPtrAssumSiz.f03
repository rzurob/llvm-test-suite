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
!*  Assumed size array
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


  PROGRAM dataPtrAssumSiz
  USE M
  IMPLICIT NONE

  TYPE(DT),     TARGET :: Arr(10,10)

  Arr = DT(-1)

  CALL S(Arr, 10, 0, 9)

  CONTAINS

  SUBROUTINE S(Arr, N, L, U)
  CLASS(*), TARGET  :: Arr(L:U, L:*)
  INTEGER           :: N, L, U
  CLASS(*), POINTER :: Ptr(:, :)


  Ptr(L:, L:) => Arr(:,L:U)

  IF (.NOT. ASSOCIATED(Ptr, Arr(:, L:U)))          ERROR STOP 11
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L /)))    ERROR STOP 12
  IF (ANY( UBOUND(Ptr)         .NE. (/U, U /)))    ERROR STOP 13
  SELECT TYPE (Ptr)
  TYPE IS (DT)
    IF (ANY( Ptr%ID            .NE.   -1))         ERROR STOP 14
    IF (ANY( Ptr%ModFun()      .NE.   -1))         ERROR STOP 15
  CLASS DEFAULT
    STOP 16
  END SELECT

  Ptr(L:U, U:U) => Arr(:,L)

  IF (.NOT. ASSOCIATED(Ptr))                       ERROR STOP 21
  IF (ANY( LBOUND(Ptr)         .NE. (/L, U /)))    ERROR STOP 22
  IF (ANY( UBOUND(Ptr)         .NE. (/U, U /)))    ERROR STOP 23
  SELECT TYPE (Ptr)
  TYPE IS (DT)
    IF (ANY( Ptr%ID            .NE.   -1))         ERROR STOP 24
    IF (ANY( Ptr%ModFun()      .NE.   -1))         ERROR STOP 25
  CLASS DEFAULT
    STOP 26
  END SELECT


  END SUBROUTINE

  END


