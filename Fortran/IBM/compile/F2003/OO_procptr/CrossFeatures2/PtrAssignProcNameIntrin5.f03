! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 14, 2005
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
!*  C727 (R742) A procedure-name shall be the name of an external, module,
!*  or dummy procedure, a specific intrinsic function listed in 13.6
!*  and not marked with a bullet (.), or a procedure pointer.
!*
!*  Intrinsics
!*  (315208)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM PtrAssignProcNameIntrin5
  IMPLICIT NONE

  INTRINSIC SIGN, SIN, SINH, SQRT, TAN, TANH

  PROCEDURE(SIGN) :: ExtSIGN
  PROCEDURE(SIN)  :: ExtSIN
  PROCEDURE(SINH) :: ExtSINH
  PROCEDURE(SQRT) :: ExtSQRT
  PROCEDURE(TAN)  :: ExtTan
  PROCEDURE(TANH) :: ExtTANH

! It seems that ExtXXX lost the elemental attribute. Compiler should complain
! the following usage
  TYPE :: DT
    PROCEDURE(ExtSIGN),  POINTER, NOPASS :: PtrSIGN
    PROCEDURE(ExtSIN),   POINTER, NOPASS :: PtrSIN
    PROCEDURE(ExtSINH),  POINTER, NOPASS :: PtrSINH
    PROCEDURE(ExtSQRT),  POINTER, NOPASS :: PtrSQRT
    PROCEDURE(ExtTAN),   POINTER, NOPASS :: PtrTan
    PROCEDURE(ExtTANH),  POINTER, NOPASS :: PtrTANH
  END TYPE

  TYPE (DT) :: V

    V%PtrSIGN => SIGN
    IF (V%PtrSIGN(-3.0, 2.0) .NE. SIGN(-3.0, 2.0) ) ERROR STOP 11

    V%PtrSIN => SIN
    IF (ABS(V%PtrSIN(1.0) - SIN(1.0)) .GT. .00001 ) ERROR STOP 12

    V%PtrSINH => SINH
    IF (ABS(V%PtrSINH(1.0) - SINH(1.0) ) .GT. .00001 ) ERROR STOP 13

    V%PtrSQRT => SQRT
    IF (ABS(V%PtrSQRT(4.0) - SQRT(4.0) ) .GT. .00001 ) ERROR STOP 14

    V%PtrTAN => TAN
    IF (ABS(V%PtrTAN(1.0) - TAN(1.0) ) .GT. .00001 ) ERROR STOP 15

    V%PtrTANH => TANH
    IF (ABS(V%PtrTANH(1.0) - TANH(1.0) ) .GT. .00001 ) ERROR STOP 16

  END

