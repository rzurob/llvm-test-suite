! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/Forall3.f
! opt variations: -qnol

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
!*  (ICE-305693)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE Assign
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    PURE SUBROUTINE Assign (Arg1, Arg2)
    TYPE (DT(*,4)), INTENT (OUT) :: Arg1
    TYPE (DT(*,4)), INTENT (IN)  :: Arg2
      Arg1%Id = Arg2%Id
      Arg1%ProcPtr => Arg2%ProcPtr
    END SUBROUTINE

    PURE FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER, INTENT(IN) :: Arg
      Fun = Arg
    END FUNCTION

    PURE FUNCTION Fun1(Arg)
    TYPE(DT(20,4)) :: Fun1
    TYPE(DT(*,4)), INTENT(IN) :: Arg
      Fun1 = Arg
    END FUNCTION

  END MODULE


  PROGRAM Forall3
  USE M
  IMPLICIT NONE

  TYPE (DT(20,4)) :: V, W(30000), U(30000), X(30000), Z(30000)
  INTEGER :: I

  PROCEDURE(Fun1), POINTER :: ProcPtr

  V%Id = 1
  V%ProcPtr => Fun
  ProcPtr => Fun1

  FORALL (I=V%ProcPtr(1):V%ProcPtr(30000):V%ProcPtr(1))

    W(I)%Id = V%ProcPtr(-1)
    W(I)%ProcPtr =>  V%ProcPtr
    U(I) = ProcPtr(DT(20,4)(-1, W(I)%ProcPtr))
    X(W(I)%ProcPtr(I))%Id = W(I)%ProcPtr(I)
    X(W(I)%ProcPtr(I))%ProcPtr => Fun
    Z(W(I)%ProcPtr(I)) = DT(20,4)(W(I)%ProcPtr(I), Fun)

  END FORALL

  DO I=Fun(1), Fun(30000), 3

    IF ( W(I)%Id .NE. -1 ) ERROR STOP 11
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr) ) ERROR STOP 12
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun) ) ERROR STOP 13

    IF ( U(I)%Id .NE. -1 ) ERROR STOP 21
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr) ) ERROR STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun) ) ERROR STOP 23

    IF ( X(I)%Id .NE. I ) ERROR STOP 31
    IF ( .NOT. ASSOCIATED(X(I)%ProcPtr) ) ERROR STOP 32
    IF ( .NOT. ASSOCIATED(X(I)%ProcPtr, Fun) ) ERROR STOP 33

    IF ( Z(I)%Id .NE. I ) ERROR STOP 41
    IF ( .NOT. ASSOCIATED(Z(I)%ProcPtr) ) ERROR STOP 42
    IF ( .NOT. ASSOCIATED(Z(I)%ProcPtr, Fun) ) ERROR STOP 43

  END DO

  END


