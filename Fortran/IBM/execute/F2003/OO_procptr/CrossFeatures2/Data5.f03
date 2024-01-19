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
!*  Initialize proc-ptr in block data.
!*
!*  (314894/314836)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: DT
      SEQUENCE
      INTEGER :: Id
      PROCEDURE(),     POINTER, NOPASS :: ProcPtr1
      PROCEDURE(IFun), POINTER, PASS   :: ProcPtr2
    END TYPE

  CONTAINS

    FUNCTION IFUN(Arg)
    TYPE(DT) :: IFun, Arg
      IFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM Data5
  USE M
  IMPLICIT NONE

  INTEGER :: I

  TYPE (DT) :: V(3)
  TYPE (DT) :: U(3)
  TYPE (DT) :: W(3)


  COMMON /B1/V
  COMMON /B2/U
  COMMON /B3/W

  DO I = 1, 3

    IF (ASSOCIATED(V(I)%ProcPtr1))   ERROR STOP 11
    IF (ASSOCIATED(V(I)%ProcPtr2))   ERROR STOP 12
    IF (V(I)%Id .NE. 1)              ERROR STOP 13

    IF (ASSOCIATED(U(I)%ProcPtr1))   ERROR STOP 21
    IF (ASSOCIATED(U(I)%ProcPtr2))   ERROR STOP 22
    IF (ANY(U%Id .NE. 2))            ERROR STOP 23

    IF (ASSOCIATED(W(I)%ProcPtr1))   ERROR STOP 31
    IF (ASSOCIATED(W(I)%ProcPtr2))   ERROR STOP 32
    IF (W(I)%Id .NE. 3)              ERROR STOP 33

  END DO

  END

  BLOCK DATA
  USE M

  TYPE (DT) :: V(3)
  TYPE (DT) :: U(3)
  TYPE (DT) :: W(3)

  COMMON /B1/V
  DATA V /3*DT(1, NULL(), NULL())/

  COMMON /B2/U
! DATA (U(I)%Id, I=1,3 ) /3*2/
! DATA (U(I)%ProcPtr1, I=1,3 ) /3*NULL()/   !not allowed?
! DATA (U(I)%ProcPtr2, I=1,3 ) /3*NULL()/   !not allowed
 DATA U /3*DT(2, NULL(), NULL())/

  COMMON /B3/W
  DATA (W(I), I=1,3,2 ) /2*DT(3, NULL(), NULL())/
  DATA (W(I), I=2,3,2 ) /1*DT(3, NULL(), NULL())/

  END BLOCK DATA



