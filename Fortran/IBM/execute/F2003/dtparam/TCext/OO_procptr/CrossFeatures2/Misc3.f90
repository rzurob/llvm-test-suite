! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/CrossFeatures2/Misc3.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 29, 2005
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
!*  Procedure pointer - implicit interface and intrinsics
!*  (315059)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc3

  IMPLICIT REAL(P)
  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    PROCEDURE(REAL), NOPASS, POINTER :: ProcPtr
  END TYPE

  INTERFACE
    FUNCTION RToR(Arg)
      REAL, INTENT(IN) :: Arg
      REAL             :: RToR
    END FUNCTION
  END INTERFACE

  PROCEDURE(),            POINTER :: ProcPtr0
  PROCEDURE(REAL),        POINTER :: ProcPtr1
  !PROCEDURE(ProcPtr1),   POINTER :: ProcPtr2 ! This is wrong
  PROCEDURE(RToR),        POINTER :: ProcPtr2
  PROCEDURE(ProcPtr2),    POINTER :: ProcPtr3
  TYPE(DT(4))                     :: V

  ProcPtr0 => SIN
  IF ( ABS(ProcPtr0(1.0)-0.84147098) .GT. 0.000001) STOP 10

  ProcPtr1 => SIN
  IF ( ABS(ProcPtr1(1.0)-0.84147098) .GT. 0.000001) STOP 11

! ProcPtr2 => SIN
! IF ( ABS(ProcPtr2(1.0)-0.84147098) .GT. 0.000001) STOP 12

  ProcPtr3 => SIN
  IF ( ABS(ProcPtr3(1.0)-0.84147098) .GT. 0.000001) STOP 13

  V%ProcPtr => SIN
  IF ( ABS(V%ProcPtr(1.0)-0.84147098) .GT. 0.000001) STOP 14

  END

