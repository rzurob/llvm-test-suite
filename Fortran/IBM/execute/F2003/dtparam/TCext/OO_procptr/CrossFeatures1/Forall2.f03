! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/Forall2.f
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
!*  FORALL/defined operator
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun1), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

    INTERFACE OPERATOR ( + )
      MODULE PROCEDURE OP
    END INTERFACE OPERATOR ( + )

    CONTAINS

    PURE FUNCTION Op (Arg1, Arg2)
    TYPE (DT(*,4)), INTENT (IN) :: Arg1
    TYPE (DT(*,4)), INTENT (IN) :: Arg2
    TYPE (DT(20,4)) :: Op
      Op.Id = Arg1%Id + Arg2%Id
      Op%ProcPtr => Arg2%ProcPtr
    END FUNCTION

    PURE FUNCTION Fun(Arg)
    TYPE(DT(20,4)) :: Fun
    TYPE(DT(*,4)), INTENT(IN) :: Arg
      !Fun = Arg
      Fun%ID = Arg%ID
      Fun%ProcPtr => Arg%ProcPtr
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

  INTERFACE
    PURE FUNCTION IFun(Arg)
    IMPORT
    TYPE(DT(20,4)) :: IFun
    TYPE(DT(*,4)), INTENT(IN) :: Arg
    END FUNCTION
  END INTERFACE

  TYPE (DT(20,4)) :: V, W(30), U(30)
  INTEGER   :: I
  PROCEDURE(IFun), POINTER :: ProcPtr

  V%Id = 1
  V%ProcPtr => Fun1
  ProcPtr => Fun

  FORALL (I=V%ProcPtr(1):V%ProcPtr(30):V%ProcPtr(1))
    W(I) = ProcPtr(V) + ProcPtr(DT(20,4)(3,Fun1))
    !U(I) = V%ProcPtr(DT(3,Fun1)) + V%ProcPtr(DT(1,Fun1))
    U(I) = DT(20,4)(3,Fun1) + DT(20,4)(1,Fun1)
  END FORALL

  DO I=1, 30

    IF ( W(I)%Id .NE. 4 ) ERROR STOP 11
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr) ) ERROR STOP 12
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun1) ) ERROR STOP 13

    IF ( U(I)%Id .NE. 4 ) ERROR STOP 21
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr) ) ERROR STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun1) ) ERROR STOP 23

  END DO

  FORALL (I=Fun1(1):Fun1(15)+Fun1(15):1)
    W(I) = DT(20,4)(1, V%ProcPtr)+ DT(20,4)(1, Fun1)
    U(I) = W(I)
  END FORALL

  DO I=1, 30

    IF ( W(I)%Id .NE. 2 ) ERROR STOP 31
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr) ) ERROR STOP 32
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun1) ) ERROR STOP 33

    IF ( W(I)%Id .NE. 2 ) ERROR STOP 41
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr) ) ERROR STOP 42
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun1) ) ERROR STOP 43

  END DO

  END

