! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures2/Forall.f
! opt variations: -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Forall.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Forall.f
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
!*  (ICE-314836)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), POINTER, PASS(Arg1) :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    PURE FUNCTION Fun(Arg1, Arg2)
    INTEGER               :: Fun
    INTEGER,   INTENT(IN) :: Arg2
    CLASS(DT(4)), INTENT(IN) :: Arg1
      Fun = Arg2
    END FUNCTION

    PURE FUNCTION Fun1(Arg)
    INTEGER  :: Fun1
    INTEGER, INTENT(IN)  :: Arg
      Fun1 = Arg
    END FUNCTION

  END MODULE

  PROGRAM Forall0
  USE M
  IMPLICIT NONE

  CLASS(DT(4)), ALLOCATABLE :: V
  TYPE (DT(4))              :: W(30), U(30)
  INTEGER :: I
  PROCEDURE(INTEGER), POINTER :: ProcPtr

  ALLOCATE(V, SOURCE=DT(4)(-1, Fun))
  ProcPtr => Fun1

  FORALL (I=V%ProcPtr(1):ProcPtr(30):V%ProcPtr(1))
    W(I)%Id = V%ProcPtr(-1)
    W(I)%ProcPtr => V%ProcPtr
    U(I) = W(I)
  END FORALL

  DO I=1, 30

    IF ( W(I)%Id .NE. -1 ) STOP 11
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr) ) STOP 12
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun) ) STOP 13

    IF ( U(I)%Id .NE. -1 ) STOP 21
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr) ) STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun) ) STOP 23

  END DO

  FORALL (I=ProcPtr(1):ProcPtr(30):ProcPtr(1))
    W(I) = DT(4)(-1, Fun)
    U(I) = W(I)
  END FORALL

  DO I=1, 30

    IF ( W(I)%Id .NE. -1 ) STOP 31
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr) ) STOP 32
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun) ) STOP 33

    IF ( W(I)%Id .NE. -1 ) STOP 41
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr) ) STOP 42
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun) ) STOP 43

  END DO

  END


