! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 12, 2005
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
!*  Var shall not be explicitly initialized more than once
!*  The check is not mandatory - only warning for some of them
!*  ()
!*   -qsuppress=1514-008 for ignoring misalignment with -q64
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      SEQUENCe
      INTEGER :: Id
      PROCEDURE(Fun), POINTER, NOPASS :: ProcPtR
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg
    CHARACTER(LEN(Arg)) :: Fun
      Fun = Arg
    END FUNCTION

  END MODULE


  PROGRAM Data
  USE M
  IMPLICIT NONE

  TYPE(DT), PARAMETER :: Const=DT(0, NULL())
  PROCEDURE(Fun), POINTER :: ProcPtr
  DATA ProcPtr /NULL(Const%ProcPtr)/

  PROCEDURE(Fun), POINTER :: ProcPtr1, ProcPtr2, ProcPtr3, ProcPtr4
  DATA ProcPtr1 /NULL(Const%ProcPtr)/, ProcPtr2 /NULL()/
  DATA ProcPtr3, ProcPtr4 /2*NULL()/
  DATA ProcPtr3 /1*NULL()/

  TYPE (DT) :: V, V1(3), V2(3)
  DATA V /DT(-1, NULL())/
  DATA V1 /3*DT(-1, NULL())/
  DATA V1(2:2) /1*DT(-1, NULL())/
  DATA V2(1:2) /2*DT(-1, NULL())/
  DATA V2(2:3) /2*DT(-1, NULL())/

  TYPE (DT) :: W, W1(3), W2(3)
  DATA W%ProcPtr / NULL()/
  DATA W1(1)%ProcPtr, W1(2)%ProcPtr, W1(3)%ProcPtr  /3*NULL()/
  DATA W1(2)%ProcPtr  /NULL()/
  DATA W2 /3*DT(0,NULL())/
  DATA W2(2) /1*DT(0,NULL())/

  COMMON ProcPtr1, ProcPtr2, ProcPtr3, ProcPtr4
! COMMON V, V1, V2
! COMMON W, W1, W2

  1  !stop compilation
  END

