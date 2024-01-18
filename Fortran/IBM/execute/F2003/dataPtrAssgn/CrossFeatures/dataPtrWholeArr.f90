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
  TYPE :: DT0
    INTEGER :: ID
  END TYPE

  TYPE, EXTENDS(DT0) :: MT
  CONTAINS
    PROCEDURE :: ModFun
  END TYPE

  CONTAINS

  ELEMENTAL FUNCTION ModFun(Arg)
  CLASS(MT), INTENT(IN) :: Arg
  INTEGER   :: ModFun
    ModFun=Arg%ID
  END FUNCTION

  END MODULE


  PROGRAM dataPtrWholeArr
  USE M, DT=>MT
  USE M, ONLY:MT
  IMPLICIT NONE

  TYPE(DT),  TARGET  :: Arr(100, 100), Arr1(10000)
  CLASS(MT), POINTER :: Ptr(:, :)
  INTEGER            :: I, J, N

  N = 100
  Arr  = DT(-1)
  Arr1 = DT(-2)


  DO I =1, 50
  DO J =I, 50

    Ptr(I:, J:) => Arr
    IF (.NOT. ASSOCIATED(Ptr,  Arr ))           STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I , J/)))       STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1,J+N-1/))) STOP 13
    IF (ANY( Ptr%ModFun() .NE. -1 ))            STOP 14

    Ptr(I:J, I:J) => Arr1
    IF (.NOT. ASSOCIATED(Ptr))                   STOP 20
    IF (SIZE(Ptr)         .NE. (J-I+1)*(J-I+1))  STOP 21
    IF (ANY( LBOUND(Ptr)  .NE. (/I , I /)))      STOP 22
    IF (ANY( UBOUND(Ptr)  .NE. (/J , J /)))      STOP 23
    IF (ANY( Ptr%ModFun() .NE. -2 ))             STOP 24

  END DO
  END DO


  END


