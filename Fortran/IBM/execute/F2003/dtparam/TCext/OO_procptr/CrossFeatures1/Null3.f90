! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/Null3.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 10, 2005
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
!*   Initialization/a structure constructor
!*  (305627) (306255)
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

  PROGRAM Null3
  USE M
  IMPLICIT NONE

  TYPE (DT(4)), PARAMETER :: V=DT(4)(-1, NULL())

  PROCEDURE(Fun), POINTER :: ProcPtr1=>NULL()
  PROCEDURE(Fun), POINTER :: ProcPtr2=>NULL()
  PROCEDURE(Fun), POINTER :: ProcPtr3=>NULL()
  PROCEDURE(Fun), POINTER :: ProcPtr4=>NULL()

  TYPE (DT(4)) :: W1=DT(4)(-1, NULL())
  TYPE (DT(4)) :: W2(3)=DT(4)(-1, NULL(V%ProcPtr))
  TYPE (DT(4)) :: W3(3)=(/DT(4)(-1, NULL()),DT(4)(-1, NULL()),DT(4)(-1, NULL()) /)
  TYPE (DT(4)) :: W4(1)=(/DT(4)(-1, NULL()) /)

  IF ( ASSOCIATED(ProcPtr1) ) ERROR STOP 11
  IF ( ASSOCIATED(ProcPtr2) ) ERROR STOP 12
  IF ( ASSOCIATED(ProcPtr3) ) ERROR STOP 13
  IF ( ASSOCIATED(ProcPtr4) ) ERROR STOP 14

  ProcPtr1 => Fun
  ProcPtr2 => Fun
  ProcPtr3 => Fun
  ProcPtr4 => Fun

  ProcPtr1=>NULL()
  ProcPtr2=>NULL(ProcPtr1)
  ProcPtr3=>NULL(V%ProcPtr)
  ProcPtr4=>NULL(ProcPtr1)

  IF ( ASSOCIATED(ProcPtr1) ) ERROR STOP 21
  IF ( ASSOCIATED(ProcPtr2) ) ERROR STOP 22
  IF ( ASSOCIATED(ProcPtr3) ) ERROR STOP 23
  IF ( ASSOCIATED(ProcPtr4) ) ERROR STOP 24

  IF ( ASSOCIATED(W1%ProcPtr) )    ERROR STOP 31
  IF ( ASSOCIATED(W2(3)%ProcPtr) ) ERROR STOP 32
  IF ( ASSOCIATED(W3(1)%ProcPtr) ) ERROR STOP 33
  IF ( ASSOCIATED(W4(1)%ProcPtr) ) ERROR STOP 34

  W1%ProcPtr => Fun
  W2 = DT(4)(-1, Fun)
  W3 = DT(4)(-1, Fun)
  W4 = DT(4)(-1, Fun)

  W1 = DT(4)(-1, NULL())
  W2 = DT(4)(-1, NULL(W1%ProcPtr))
  W3 = (/DT(4)(-1, NULL()),DT(4)(-1, NULL()),DT(4)(-1, NULL()) /)
  W4 = (/DT(4)(-1, NULL(W3(1)%ProcPtr)) /)

  IF ( ASSOCIATED(W1%ProcPtr) )    ERROR STOP 41
  IF ( ASSOCIATED(W2(2)%ProcPtr) ) ERROR STOP 42
  IF ( ASSOCIATED(W3(3)%ProcPtr) ) ERROR STOP 43
  IF ( ASSOCIATED(W4(1)%ProcPtr) ) ERROR STOP 44


  END


