! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 10, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The nullify stmt
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr=>NULL()
    CONTAINS
      PROCEDURE, NoPASS   :: BindProc => ProcFun
    END TYPE

    CHARACTER(10) :: Mark

    CONTAINS

    FUNCTION ProcFun(Arg)
    PROCEDURE(INTEGER), POINTER :: ProcFun
    PROCEDURE(INTEGER)          :: Arg
      ProcFun => Arg
    END FUNCTION

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Nullify
  USE M
  IMPLICIT NONE

  TYPE(DT)                :: V
  PROCEDURE(INTEGER), POINTER :: ProcPtr1=>NULL()
  PROCEDURE(   ), POINTER :: ProcPtr2=>NULL()
  EXTERNAL                :: ExtSub

  V%ProcPtr =>  Fun
  IF ( .NOT. ASSOCIATED( V%ProcPtr )) ERROR STOP 11
  IF (  V%ProcPtr(-1) .NE. -1)        ERROR STOP 12
  NULLIFY(V%ProcPtr)
  IF ( ASSOCIATED( V%ProcPtr ))       ERROR STOP 13

  ProcPtr1 =>  Fun
  IF ( .NOT. ASSOCIATED( ProcPtr1 ))  ERROR STOP 21
  IF (  ProcPtr1(-2) .NE. -2)         ERROR STOP 22
  NULLIFY(ProcPtr1)
  IF ( ASSOCIATED( ProcPtr1 ))        ERROR STOP 23

  ProcPtr2 => ExtSub
  IF ( .NOT. ASSOCIATED( ProcPtr2 ))  ERROR STOP 31
  CALL ProcPtr2("12345")
  IF ( Mark .NE. "12345")             ERROR STOP 32
  NULLIFY(ProcPtr2)
  IF ( ASSOCIATED( ProcPtr2 ))        ERROR STOP 33

  ProcPtr1 => V%BindProc(Fun)
  IF ( .NOT. ASSOCIATED( ProcPtr1 ))  ERROR STOP 42
  IF (  ProcPtr1(-2)   .NE. -2)       ERROR STOP 44

  V%ProcPtr => V%BindProc(Fun)
  IF ( .NOT. ASSOCIATED( V%ProcPtr))  ERROR STOP 52
  IF (  V%ProcPtr(-2)   .NE. -2)      ERROR STOP 54

  NULLIFY(V%ProcPtr, ProcPtr1)
  IF ( ASSOCIATED( V%ProcPtr ))       ERROR STOP 65
  IF ( ASSOCIATED( ProcPtr1 ))        ERROR STOP 66


  END

  SUBROUTINE ExtSub(Arg)
  USE M
  IMPLICIT NONE
  CHARACTER(*) :: Arg
    Mark = Arg
  END SUBROUTINE


