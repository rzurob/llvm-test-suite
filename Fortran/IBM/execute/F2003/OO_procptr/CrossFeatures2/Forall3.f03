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
!*  FORALL/assignments
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(Fun), POINTER, NOPASS :: ProcPtr=>NULL()
      PROCEDURE(Fun1), POINTER, PASS  :: ProcPtr1=>NULL()
    CONTAINS
      PROCEDURE, NoPASS :: Proc => Fun
      PROCEDURE, PASS :: Proc1 => Fun1
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE Assign
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    PURE SUBROUTINE Assign (Arg1, Arg2)
    TYPE (DT), INTENT (OUT) :: Arg1
    TYPE (DT), INTENT (IN)  :: Arg2
      Arg1%Id =-1
      Arg1%ProcPtr => Arg2%ProcPtr
      Arg1%ProcPtr1 => Arg2%ProcPtr1
    END SUBROUTINE

    PURE FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER, INTENT(IN) :: Arg
      Fun = Arg
    END FUNCTION

    PURE FUNCTION Fun1(Arg)
    CLASS(DT), INTENT(IN) :: Arg
    TYPE(DT) :: Fun1
      Fun1 = Arg
    END FUNCTION

  END MODULE


  PROGRAM Forall3
  USE M
  IMPLICIT NONE

  TYPE (DT) :: V, W(30000), U(30000), X(30000), Z(30000)
  INTEGER :: I

  PROCEDURE(Fun1), POINTER :: ProcPtr

  V = DT(1, Fun, Fun1)
  ProcPtr => Fun1

  FORALL (I=V%Proc(1):V%ProcPtr(30000):V%Proc(1))

    W(I)%Id = V%Proc(-1)
    W(I)%ProcPtr =>  V%ProcPtr
    W(I)%ProcPtr1 =>  V%ProcPtr1
    U(I) = ProcPtr(DT(-1, W(I)%ProcPtr, Fun1))
    X(W(I)%Proc(I))%Id = W(I)%ProcPtr(I)
    X(W(I)%ProcPtr(I))%ProcPtr => Fun
    X(W(I)%ProcPtr(I))%ProcPtr1 => Fun1
    Z(W(I)%ProcPtr(I)) = DT(W(I)%ProcPtr(I), Fun, Fun1)

  END FORALL

  DO I=Fun(1), Fun(30000), 1

    IF ( W(I)%Id .NE. -1 ) ERROR STOP 11
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun) ) ERROR STOP 12
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr1, Fun1) ) ERROR STOP 13

    IF ( U(I)%Id .NE. -1 ) ERROR STOP 21
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun) ) ERROR STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr1, Fun1) ) ERROR STOP 23

    IF ( X(I)%Id .NE. I ) ERROR STOP 31
    IF ( .NOT. ASSOCIATED(X(I)%ProcPtr, Fun) ) ERROR STOP 32
    IF ( .NOT. ASSOCIATED(X(I)%ProcPtr1, Fun1) ) ERROR STOP 33

    IF ( Z(I)%Id .NE. -1 ) ERROR STOP 41
    IF ( .NOT. ASSOCIATED(Z(I)%ProcPtr, Fun) ) ERROR STOP 42
    IF ( .NOT. ASSOCIATED(Z(I)%ProcPtr1, Fun1) ) ERROR STOP 43

  END DO

  END


