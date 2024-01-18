! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 13, 2005
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
!*  (314745/315019/316122/328094.test)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Arg2
  IMPLICIT NONE

  PROCEDURE(ABS)   :: ExtABS
  PROCEDURE(ACOS)  :: ExtACOS
  PROCEDURE(AIMAG) :: ExtAIMAG
  PROCEDURE(AINT)  :: ExtAINT
  PROCEDURE(ALOG)  :: ExtALOG
  PROCEDURE(ALOG10):: ExtALOG10
  PROCEDURE(AMOD)  :: ExtAMOD
  PROCEDURE(ANINT) :: ExtANINT
  PROCEDURE(ASIN)  :: ExtASIN
  PROCEDURE(ATAN)  :: ExtATAN

  INTRINSIC   ABS
  INTRINSIC   ACOS
  INTRINSIC   AIMAG
  INTRINSIC   AINT
  INTRINSIC   ALOG
  INTRINSIC   ALOG10
  INTRINSIC   AMOD
  INTRINSIC   ANINT
  INTRINSIC   ASIN
  INTRINSIC   ATAN


  IF (ExtABS(-1.0) .NE.  1.0  ) STOP 11

  IF (ABS(ExtACOS(0.54030231) - 1.0) .GT. .00001 ) STOP 12

  IF (ExtAIMAG((2.0, -1.0)) .NE. -1.0 )   STOP 13

  IF (ExtAINT(-2.783) .NE. -2.0 )   STOP 14

  IF (ExtALOG(1.0) .NE. 0.0 ) STOP 15

  IF (ExtALOG10(1.0) .NE. 0.0 ) STOP 16

  IF (ExtAMOD(3.0, 2.0) .NE. 1.0 ) STOP 17

  IF (ExtANINT(2.783) .NE. 3.0 ) STOP 18

  IF (ABS(ExtASIN(0.84147098)-1.0) .GT. .00001 ) STOP 19

  IF (ABS(ExtATAN(1.5574077)-1.0) .GT. .00001 ) STOP 20


  END

  ELEMENTAL REAL FUNCTION ExtABS(Arg)
  REAL, INTENT(IN) :: Arg
    ExtABS = ABS(Arg)
  END FUNCTION

  ELEMENTAL REAL FUNCTION ExtACOS(Arg)
  REAL, INTENT(IN) :: Arg
    ExtACOS = ACOS(Arg)
  END FUNCTION

  ELEMENTAL REAL FUNCTION ExtAIMAG(Arg)
  COMPLEX, INTENT(IN) :: Arg
    ExtAIMAG = AIMAG(Arg)
  END FUNCTION

  ELEMENTAL REAL FUNCTION ExtAINT(Arg)
  REAL, INTENT(IN) :: Arg
    ExtAINT = AINT(Arg)
  END FUNCTION

  ELEMENTAL REAL FUNCTION ExtALOG(Arg)
  REAL, INTENT(IN) :: Arg
    ExtALOg = ALOG(Arg)
  END FUNCTION

  ELEMENTAL REAL FUNCTION ExtALOG10(Arg)
  REAL, INTENT(IN) :: Arg
    ExtALOG10 = ALOG10(Arg)
  END FUNCTION

  ELEMENTAL REAL FUNCTION ExtAMOD(Arg1, Arg2)
  REAL, INTENT(IN) :: Arg1, Arg2
    ExtAMOD = AMOD(Arg1, Arg2)
  END FUNCTION

  ELEMENTAL REAL FUNCTION ExtANINT(Arg)
  REAL, INTENT(IN) :: Arg
    ExtANINT = ANINT(Arg)
  END FUNCTION

  ELEMENTAL REAL FUNCTION ExtASIN(Arg)
  REAL, INTENT(IN) :: Arg
    ExtASIn = aSIN(Arg)
  END FUNCTION

  ELEMENTAL REAL FUNCTION ExtATAN(Arg)
  REAL, INTENT(IN) :: Arg
    ExtATAN = ATAN(Arg)
  END FUNCTION


