! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures2/Data4.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 22, 2005
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
!*  Initialize proc-ptr component of an object in data stmt.
!*
!*  (314894/315287)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), PASS, POINTER :: ProcPtr1
      PROCEDURE(Fun), PASS, POINTER :: ProcPtr2
      PROCEDURE(Fun), PASS, POINTER :: ProcPtr3
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    TYPE(DT(4))  :: Fun
    CLASS(DT(4)) :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE


  PROGRAM Data4
  USE M
  IMPLICIT NONE

  INTEGER :: I

  TYPE (DT(4)) :: V(3)
  DATA V /3*DT(4)(-1,NULL(),NULL(),NULL())/

  TYPE (DT(4)) :: U(3)
  DATA (U(I)%Id, I=1,3 ) /3*-1/
  DATA (U(I)%ProcPtr1, I=1,3 ) /3*NULL()/
  DATA (U(I)%ProcPtr2, I=1,3 ) /3*NULL()/
  DATA (U(I)%ProcPtr3, I=1,3 ) /3*NULL()/

  TYPE (DT(4)) :: W(3)
  DATA (W(I), I=1,3,2 ) /2*DT(4)(-1, NULL(),NULL(),NULL())/
  DATA (W(I), I=2,3,2 ) /1*DT(4)(-1, NULL(),NULL(),NULL())/


  IF (ASSOCIATED(V(1)%ProcPtr1)) ERROR STOP 11
  IF (ASSOCIATED(V(2)%ProcPtr2)) ERROR STOP 11
  IF (ASSOCIATED(V(3)%ProcPtr3)) ERROR STOP 11
  IF (ANY(V%Id .NE. -1))         ERROR STOP 12

  IF (ASSOCIATED(U(1)%ProcPtr1)) ERROR STOP 21
  IF (ASSOCIATED(U(2)%ProcPtr2)) ERROR STOP 21
  IF (ASSOCIATED(U(3)%ProcPtr3)) ERROR STOP 21
  IF (ANY(U%Id .NE. -1))         ERROR STOP 22

  IF (ASSOCIATED(W(1)%ProcPtr1)) ERROR STOP 31
  IF (ASSOCIATED(W(2)%ProcPtr2)) ERROR STOP 31
  IF (ASSOCIATED(W(3)%ProcPtr3)) ERROR STOP 31
  IF (ANY(W%Id .NE. -1))         ERROR STOP 32

  END


