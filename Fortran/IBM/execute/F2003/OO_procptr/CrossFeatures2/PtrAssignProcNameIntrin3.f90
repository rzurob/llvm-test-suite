! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: AssignProcNameIntrin3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : AssignProcNameIntrin3.f
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
!*  (315305)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  DOUBLE PRECISION FUNCTION ExtDLOG(Arg)
  DOUBLE PRECISION Arg
    ExtDLOG = DLOG(Arg)
  END FUNCTION

  DOUBLE PRECISION FUNCTION ExtDLOG10(Arg)
  DOUBLE PRECISION Arg
    ExtDLOG10 = DLOG10(Arg)
  END FUNCTION

  DOUBLE PRECISION FUNCTION ExtDMOD(Arg1, Arg2)
  DOUBLE PRECISION Arg1, Arg2
    ExtDMOD = DMOD(Arg1, Arg2)
  END FUNCTION

  DOUBLE PRECISION FUNCTION ExtDNINT(Arg)
  DOUBLE PRECISION Arg
    ExtDNINT = DNINT(Arg)
  END FUNCTION

  DOUBLE PRECISION FUNCTION ExtDPROD(Arg1, Arg2)
  REAL :: Arg1, Arg2
    ExtDPROD = DPROD(Arg1, Arg2)
  END FUNCTION

  DOUBLE PRECISION FUNCTION ExtDSIGN(Arg1, Arg2)
  DOUBLE PRECISION Arg1, Arg2
    ExtDSIGN = DSIGN(Arg1, Arg2)
  END FUNCTION

  DOUBLE PRECISION FUNCTION ExtDSIN(Arg)
  DOUBLE PRECISION Arg
    ExtDSIN = DSIN(Arg)
  END FUNCTION

  DOUBLE PRECISION FUNCTION ExtDSINH(Arg)
  DOUBLE PRECISION Arg
    ExtDSINH = DSINH(Arg)
  END FUNCTION

  DOUBLE PRECISION FUNCTION ExtDSQRT(Arg)
  DOUBLE PRECISION Arg
    ExtDSQRT = DSQRT(Arg)
  END FUNCTION

  DOUBLE PRECISION FUNCTION ExtDTAN(Arg)
  DOUBLE PRECISION Arg
    ExtDTAN = DTAN(Arg)
  END FUNCTION


  PROGRAM AssignProcNameIntrin3
  IMPLICIT NONE

  PROCEDURE(DLOG)   :: ExtDLOG
  PROCEDURE(DLOG10) :: ExtDLOG10
  PROCEDURE(DMOD)   :: ExtDMOD
  PROCEDURE(DNINT)  :: ExtDNINT
  PROCEDURE(DPROD)  :: ExtDPROD
  PROCEDURE(DSIGN)  :: ExtDSIGN
  PROCEDURE(DSIN)   :: ExtDSIN
  PROCEDURE(DSINH)  :: ExtDSINH
  PROCEDURE(DSQRT)  :: ExtDSQRT
  PROCEDURE(DTAN)   :: ExtDTAN


  IF (ABS(ExtDLOG(10.0D0) - 2.3025851D0 ) .GT. .00001 ) STOP 11

  IF (ABS(ExtDLOG10(10.0D0) - 1.0D0 ) .GT. .00001 ) STOP 12

  IF (ABS(ExtDMOD(3.0D0, 2.0D0) - 1.0D0 ) .GT. .00001 ) STOP 13

  IF (ExtDNINT(2.783D0) .NE. 3.D0) STOP 14

  IF (ExtDPROD(3.0, 2.0) .NE. 6.0D0 ) STOP 15

  IF (ExtDSIGN(-3.0D0, 2.0D0) .NE. 3.0D0  ) STOP 16

  IF (ABS(ExtDSIN(1.0D0) - 0.84147098D0 ) .GT. .00001 ) STOP 17

  IF (ABS(ExtDSINH(1.0D0) - 1.1752012D0 ) .GT. .00001 ) STOP 18

  IF (ABS(ExtDSQRT(4.0D0) - 2.D0) .GT. .00001 ) STOP 19

  IF (ABS(ExtDTAN(1.0D0) - 1.5574077D0) .GT. .00001 ) STOP 20

  END

