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
!*  type bound procedure
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0
    CHARACTER(3) :: CArr(0:9, 0:9)
  END TYPE

  TYPE, EXTENDS(DT0) :: DT
  CONTAINS
    PROCEDURE, NOPASS :: Fun => ModFun
    PROCEDURE, NOPASS :: Fun1 => ModFun1
  END TYPE

  TYPE (DT), SAVE, TARGET   :: T

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT),  TARGET :: Arg
  CHARACTER(3), POINTER :: ModFun(:, :)
    ModFun(LBOUND(Arg%CArr,1):,LBOUND(Arg%CArr,2):) => Arg%CArr
  END FUNCTION

  FUNCTION ModFun1(Arg)
  CLASS(DT),  TARGET, INTENT(IN) :: Arg
  CHARACTER(3), POINTER :: ModFun1(:)
    ModFun1(LBOUND(Arg%CArr,1):UBOUND(Arg%CArr,1)) => Arg%CArr(:, 1)
  END FUNCTION

  END MODULE

  PROGRAM dataPtrTypeBound
  USE M
  IMPLICIT NONE

  CHARACTER(3), POINTER :: PtrCArr(:, :)

  T%CArr = "123"
  PtrCArr => T%Fun(T)

  IF (.NOT. ASSOCIATED(PtrCArr))                       ERROR STOP 11
  IF (ANY( LBOUND(PtrCArr)         .NE. (/0, 0 /)))    ERROR STOP 12
  IF (ANY( UBOUND(PtrCArr)         .NE. (/9, 9 /)))    ERROR STOP 13
  IF (ANY( PtrCArr                 .NE. "123"))        ERROR STOP 14

  PtrCArr(1:, 1:) => T%Fun(T)
  IF (.NOT. ASSOCIATED(PtrCArr))                       ERROR STOP 21
  IF (ANY( LBOUND(PtrCArr)         .NE. (/ 1, 1 /)))   ERROR STOP 22
  IF (ANY( UBOUND(PtrCArr)         .NE. (/10,10 /)))   ERROR STOP 23
  IF (ANY( PtrCArr                 .NE. "123"))        ERROR STOP 24

  PtrCArr(0:9, 0:0 ) => T%Fun1(T)
  IF (.NOT. ASSOCIATED(PtrCArr))                       ERROR STOP 15
  IF (ANY( LBOUND(PtrCArr)         .NE. (/0, 0 /)))    ERROR STOP 16
  IF (ANY( UBOUND(PtrCArr)         .NE. (/9, 0 /)))    ERROR STOP 17
  IF (ANY( PtrCArr                 .NE. "123"))        ERROR STOP 18

  END

