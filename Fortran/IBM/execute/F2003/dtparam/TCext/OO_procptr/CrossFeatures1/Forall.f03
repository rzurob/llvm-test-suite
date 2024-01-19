! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/Forall.f
! opt variations: -ql

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
!*  FORALL/HEADER
!*
!*  (ICE-304566)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Forall
  USE M
  IMPLICIT NONE

  TYPE (DT(4)) :: V, W(30000), U(30000)
  INTEGER :: I
  PROCEDURE(Fun), POINTER :: ProcPtr

  V%Id = -1
  V%ProcPtr => Fun
  ProcPtr => Fun

  FORALL (I=V%ProcPtr(1):ProcPtr(30000):V%ProcPtr(1))
    W(I) = V
    U(I)%ProcPtr => ProcPtr
  END FORALL

  DO I=1, 30000

    IF ( W(I)%Id .NE. -1 ) ERROR STOP 11
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr) ) ERROR STOP 12
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun) ) ERROR STOP 13
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, ProcPtr) ) ERROR STOP 14

    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr) ) ERROR STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun) ) ERROR STOP 23
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, ProcPtr) ) ERROR STOP 24

  END DO

  FORALL (I=ProcPtr(1):ProcPtr(30000):ProcPtr(1))
    W(I) = DT(4)(-1, PRocPtr)
    U(I) = W(I)
  END FORALL

  DO I=1, 30000

    IF ( W(I)%Id .NE. -1 ) ERROR STOP 31
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr) ) ERROR STOP 32
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun) ) ERROR STOP 33
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, ProcPtr) ) ERROR STOP 34

    IF ( W(I)%Id .NE. -1 ) ERROR STOP 41
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr) ) ERROR STOP 42
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun) ) ERROR STOP 43
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, ProcPtr) ) ERROR STOP 44

  END DO

  END


