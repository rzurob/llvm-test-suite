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
!*  FORALL/defined assignment
!*
!*  (304672)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER                             :: Id
      PROCEDURE(Fun1),    POINTER, NOPASS :: ProcPtr=>NULL()
      PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr1=>NULL()
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE Assign
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    PURE SUBROUTINE Assign (Arg1, Arg2)
    TYPE (DT), INTENT (OUT) :: Arg1
    TYPE (DT), INTENT (IN)  :: Arg2
      Arg1%Id = Arg2%ID
      Arg1%ProcPtr => Arg2%ProcPtr
      Arg1%ProcPtr1 => Arg2%ProcPtr1
    END SUBROUTINE

    PURE FUNCTION Fun(Arg)
    TYPE(DT) :: Fun
    TYPE(DT), INTENT(IN) :: Arg
      Fun = Arg
    END FUNCTION

    PURE FUNCTION Fun1(Arg)
    INTEGER :: Fun1
    INTEGER, INTENT(IN) :: Arg
      Fun1 = Arg
    END FUNCTION

  END MODULE


  PROGRAM Forall1
  USE M
  IMPLICIT NONE

  TYPE (DT) :: V, W(3000), U(3000)
  INTEGER :: I,  IArr(3000)
  PROCEDURE(Fun), POINTER :: ProcPtr

  V = DT(-1, Fun1, Fun1)
  ProcPtr => Fun

  FORALL (I=V%ProcPtr1(1):V%ProcPtr1(3000):V%ProcPtr1(1))
    IArr(I) = V%ProcPtr(-1)
    U(I) = ProcPtr(V)
  END FORALL

  DO I=1, 3000

    IF ( IArr(I) .NE. -1 ) ERROR STOP 11

    IF ( U(I)%Id .NE. -1 )                       ERROR STOP 21
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr) )        ERROR STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun1) )  ERROR STOP 23
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr1, Fun1) ) ERROR STOP 24

  END DO

  FORALL (I=V%ProcPtr1(1):V%ProcPtr1(3000):V%ProcPtr1(1))
    W(I) = DT(1, V%ProcPtr, V%ProcPtr1)
    U(I) = W(I)
  END FORALL

  DO I=1, 3000

    IF ( W(I)%Id .NE. 1 )                        ERROR STOP 31
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr) )        ERROR STOP 32
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun1) )  ERROR STOP 33
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr1, Fun1) ) ERROR STOP 34

    IF ( U(I)%Id .NE.  1 )                       ERROR STOP 41
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr) )        ERROR STOP 42
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun1) )  ERROR STOP 43
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr1, Fun1) ) ERROR STOP 44

  END DO

  END

