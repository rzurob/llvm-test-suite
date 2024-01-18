! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/Data4.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 13, 2005
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
!*  Initialize proc-ptr component of an object in data stmt.
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), NOPASS, POINTER :: ProcPtr
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg
    CHARACTER(LEN(Arg)) :: Fun
      Fun = Arg
    END FUNCTION

  END MODULE


  PROGRAM Data4
  USE M
  IMPLICIT NONE

  INTEGER :: I

  TYPE (DT(20,4)) :: V(3)
  DATA V /3*DT(20,4)(-1, NULL())/

  TYPE (DT(20,4)) :: U(3)
  DATA (U(I)%Id, I=1,3 ) /3*-1/
  DATA (U(I)%ProcPtr, I=1,3 ) /3*NULL()/

  TYPE (DT(20,4)) :: W(3)
  DATA (W(I), I=1,3,2 ) /2*DT(20,4)(-1, NULL())/
  DATA (W(I), I=2,3,2 ) /1*DT(20,4)(-1, NULL())/


  IF (ASSOCIATED(V(1)%ProcPtr)) ERROR STOP 11
  IF (ASSOCIATED(V(2)%ProcPtr)) ERROR STOP 11
  IF (ASSOCIATED(V(3)%ProcPtr)) ERROR STOP 11
  IF (ANY(V%Id .NE. -1))        ERROR STOP 12

  IF (ASSOCIATED(U(1)%ProcPtr)) ERROR STOP 21
  IF (ASSOCIATED(U(2)%ProcPtr)) ERROR STOP 21
  IF (ASSOCIATED(U(3)%ProcPtr)) ERROR STOP 21
  IF (ANY(U%Id .NE. -1))        ERROR STOP 22

  IF (ASSOCIATED(W(1)%ProcPtr)) ERROR STOP 31
  IF (ASSOCIATED(W(2)%ProcPtr)) ERROR STOP 31
  IF (ASSOCIATED(W(3)%ProcPtr)) ERROR STOP 31
  IF (ANY(W%Id .NE. -1))        ERROR STOP 32

  END


