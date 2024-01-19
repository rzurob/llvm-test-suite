! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 11, 2005
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
!*   null()
!*   Actual argument/DATA statement
!*  (315124/315825)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(Fun), POINTER :: ProcPtr
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    TYPE(DT)  :: Fun
    CLASS(DT) :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Null4
  USE M
  IMPLICIT NONE

  TYPE (DT) :: V

  PROCEDURE(Fun), POINTER :: ProcPtr1
  PROCEDURE(Fun), POINTER :: ProcPtr2
  PROCEDURE(Fun), POINTER :: ProcPtr3
  PROCEDURE(Fun), POINTER :: ProcPtr4

  DATA  ProcPtr1 /NULL()/,  &
      & ProcPtr2 /NULL()/,  &
      & ProcPtr3 /NULL()/,  &
      & ProcPtr4 /NULL()/

  TYPE(DT) :: W1,W2, W3(3), W4(1)

  DATA   W1 /DT(-1, NULL())/, &
       & W2 /DT(-1, NULL())/, &
       & W3 /3*DT(-1, NULL())/, &
       & W4 /DT(-1, NULL())/


  IF ( ASSOCIATED(ProcPtr1) )      ERROR STOP 11
  IF ( ASSOCIATED(ProcPtr2) )      ERROR STOP 12
  IF ( ASSOCIATED(ProcPtr3) )      ERROR STOP 13
  IF ( ASSOCIATED(ProcPtr4) )      ERROR STOP 14


  IF ( ASSOCIATED(W1%ProcPtr) )      ERROR STOP 41
  IF ( ASSOCIATED(W2%ProcPtr) )      ERROR STOP 4
  IF ( ASSOCIATED(W3(2)%ProcPtr))    ERROR STOP 43
  IF ( ASSOCIATED(W4(1)%ProcPtr))    ERROR STOP 44


  CALL IntSub(NULL(), DT(-1, NULL()), DT(-1, NULL(V%ProcPtr)), NULL(ProcPtr1))

  CALL IntSub1(DT(-1, NULL()), &
              & DT(-1, NULL(V%ProcPtr)), &
              & (/DT(-1, NULL()),DT(-1, NULL()),DT(-1, NULL()) /), &
              & (/DT(-1, NULL(W3(1)%ProcPtr)) /))

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2, Arg3, Arg4)
  PROCEDURE(Fun), POINTER :: Arg1, Arg4
  TYPE (DT) ::Arg2, Arg3

  IF ( ASSOCIATED(Arg1) )         ERROR STOP 21
  IF ( ASSOCIATED(Arg2%ProcPtr) ) ERROR STOP 22
  IF ( ASSOCIATED(Arg3%ProcPtr) ) ERROR STOP 22
  IF ( ASSOCIATED(Arg4) )         ERROR STOP 24

  END SUBROUTINE

  SUBROUTINE IntSub1(Arg1, Arg2, Arg3, Arg4)
  TYPE (DT) ::Arg1, Arg2, Arg3(:), Arg4(1)

  IF ( ASSOCIATED(Arg1%ProcPtr) )    ERROR STOP 31
  IF ( ASSOCIATED(Arg2%ProcPtr) )    ERROR STOP 32
  IF ( ASSOCIATED(Arg3(3)%ProcPtr))  ERROR STOP 33
  IF ( ASSOCIATED(Arg4(1)%ProcPtr))  ERROR STOP 34

  END SUBROUTINE

  END


