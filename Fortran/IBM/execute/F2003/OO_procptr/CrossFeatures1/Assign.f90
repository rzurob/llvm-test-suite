! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 13, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*  The evaluation of expressions within variable shall neither affect
!*  nor be affected by the evaluation of expr
!*  This is a user's resposobility - Make it a normal invocation onto ptroc ptr
!*  (linux-306968)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(Fun), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER, INTENT(INOUT) :: Arg
    INTEGER :: Fun
      Fun = Arg
      Arg = Arg + 1
    END FUNCTION

  END MODULE


  PROGRAM Assign
  USE M
  IMPLICIT NONE

  TYPE (DT) :: V(3)
  INTEGER   :: I
  PROCEDURE(Fun), POINTER :: ProcPtr

  ProcPtr => Fun

  I =1
  V(ProcPtr(I)) = DT(I, ProcPtr)

  IF (I .NE. 2 )                             ERROR STOP 10
  IF (V(1)%Id .NE. 1 )                       ERROR STOP 11
  IF ( .NOT. ASSOCIATED(V(1)%ProcPtr, Fun) ) ERROR STOP 12

  I = 1
  V(I) = DT(ProcPtr(I), ProcPtr)
  IF (I .NE. 2 )                             ERROR STOP 20
  IF (V(2)%Id .NE. 1 )                       ERROR STOP 21
  IF ( .NOT. ASSOCIATED(V(2)%ProcPtr, Fun) ) ERROR STOP 22

  END

