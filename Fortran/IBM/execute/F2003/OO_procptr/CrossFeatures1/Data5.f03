! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 13, 2005
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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      SEQUENCE
      INTEGER :: Id
      PROCEDURE(), POINTER, NOPASS :: ProcPtr
    END TYPE
  END MODULE


  PROGRAM Data5
  USE M
  IMPLICIT NONE

  INTEGER :: I

  TYPE (DT) :: V(3)
  TYPE (DT) :: U(3)
  TYPE (DT) :: W(3)


  COMMON /B1/V
! COMMON /B2/U
  COMMON /B3/W


  IF (ASSOCIATED(V(1)%ProcPtr))   ERROR STOP 11
  IF (ASSOCIATED(V(2)%ProcPtr))   ERROR STOP 11
  IF (ASSOCIATED(V(3)%ProcPtr))   ERROR STOP 11
  IF (ANY(V%Id .NE. 1))           ERROR STOP 12

! IF (ASSOCIATED(U(1)%ProcPtr))   ERROR STOP 21
! IF (ASSOCIATED(U(2)%ProcPtr))   ERROR STOP 21
! IF (ASSOCIATED(U(3)%ProcPtr))   ERROR STOP 21
! IF (ANY(U%Id .NE. 2))           ERROR STOP 22

  IF (ASSOCIATED(W(1)%ProcPtr))   ERROR STOP 31
  IF (ASSOCIATED(W(2)%ProcPtr))   ERROR STOP 32
  IF (ASSOCIATED(W(3)%ProcPtr))   ERROR STOP 33
  IF (ANY(W%Id .NE. 3))           ERROR STOP 34

  END

  BLOCK DATA
  USE M

  TYPE (DT) :: V(3)
! TYPE (DT) :: U(3)
  TYPE (DT) :: W(3)

  COMMON /B1/V
  DATA V /3*DT(1, NULL())/

! COMMON /B2/U
! DATA (U(I)%Id, I=1,3 ) /3*2/
! DATA (U(I)%ProcPtr, I=1,3 ) /3*NULL()/   !not allowed

  COMMON /B3/W
  DATA (W(I), I=1,3,2 ) /2*DT(3, NULL())/
  DATA (W(I), I=2,3,2 ) /1*DT(3, NULL())/

  END BLOCK DATA


