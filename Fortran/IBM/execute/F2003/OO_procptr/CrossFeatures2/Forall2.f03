! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 21, 2005
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
!*  FORALL/defined operator
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(Fun1), POINTER, NOPASS   :: ProcPtr=>NULL()
      !PROCEDURE(IFun), POINTER, PASS(Arg) :: ProcPtr1=>NULL()
      PROCEDURE(Fun), POINTER, PASS(Arg) :: ProcPtr1=>NULL()
    END TYPE

    INTERFACE OPERATOR ( + )
      MODULE PROCEDURE OP
    END INTERFACE OPERATOR ( + )

    INTERFACE
      PURE FUNCTION IFun(Arg)
      IMPORT
      TYPE(DT) :: IFun
      CLASS(DT), INTENT(IN) :: Arg
      END FUNCTION
    END INTERFACE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE Assign
    END INTERFACE ASSIGNMENT ( = )
    CONTAINS

    PURE FUNCTION Op (Arg1, Arg2)
    TYPE (DT), INTENT (IN) :: Arg1
    TYPE (DT), INTENT (IN) :: Arg2
    TYPE (DT) :: Op
      Op%Id = Arg1%Id + Arg2%Id
      Op%ProcPtr => Arg2%ProcPtr
      Op%ProcPtr1 => Arg2%ProcPtr1
    END FUNCTION

    PURE SUBROUTINE Assign (Arg1, Arg2)
    TYPE (DT), INTENT (OUT) :: Arg1
    TYPE (DT), INTENT (IN)  :: Arg2
      Arg1%Id = Arg2%ID
      Arg1%ProcPtr => Arg2%ProcPtr
      Arg1%ProcPtr1 => Arg2%ProcPtr1
    END SUBROUTINE

    PURE FUNCTION Fun(Arg)
    TYPE(DT) :: Fun
    CLASS(DT), INTENT(IN) :: Arg
      Fun = Arg
    END FUNCTION

    PURE FUNCTION Fun1(Arg)
    INTEGER :: Fun1
    INTEGER, INTENT(IN) :: Arg
      Fun1 = Arg
    END FUNCTION

  END MODULE


  PROGRAM Forall2
  USE M
  IMPLICIT NONE

  TYPE (DT) :: V, W(30), U(30)
  INTEGER   :: I
  PROCEDURE(IFun), POINTER :: ProcPtr

  V%Id = 1
  V%ProcPtr => Fun1
  V%ProcPtr1 => Fun
  ProcPtr => Fun

  FORALL (I=V%ProcPtr(1):V%ProcPtr(30):V%ProcPtr(1))
    W(I) = ProcPtr(V) + ProcPtr(DT(3,Fun1, Fun))
    U(I) = V%ProcPtr1() + V%ProcPtr1()
  END FORALL

  DO I=1, 30

    IF ( W(I)%Id .NE. 4 ) ERROR STOP 11
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun1) ) ERROR STOP 12
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr1, Fun) ) ERROR STOP 13

    IF ( U(I)%Id .NE. 2 ) ERROR STOP 21
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun1) ) ERROR STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr1, Fun) ) ERROR STOP 23

  END DO

  FORALL (I=Fun1(1):Fun1(15)+Fun1(15):1)
    W(I) = DT(1, V%ProcPtr, V%ProcPtr1)+ DT(1, Fun1, Fun)
    U(I) = W(I)
  END FORALL

  DO I=1, 30

    IF ( W(I)%Id .NE. 2 ) ERROR STOP 31
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun1) ) ERROR STOP 32
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr1, Fun) ) ERROR STOP 33

    IF ( W(I)%Id .NE. 2 ) ERROR STOP 41
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun1) ) ERROR STOP 42
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr1, Fun) ) ERROR STOP 43

  END DO

  END

