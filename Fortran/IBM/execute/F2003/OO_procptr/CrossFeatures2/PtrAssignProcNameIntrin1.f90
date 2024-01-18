! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: ExtAssignProcNameIntrin1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ExtAssignProcNameIntrin1.f
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
!*  Intrinsincs
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  FUNCTION ExtATAN2(Arg1, Arg2)
  REAL :: ExtATAN2
  REAL, INTENT(IN) :: Arg1, Arg2
    ExtATAN2 = ATAN2(Arg1, Arg2)
  END FUNCTION

  FUNCTION ExtCABS(Arg)
  REAL :: ExtCABS
  COMPLEX, INTENT(IN) :: Arg
    ExtCABS = CABS(Arg)
  END FUNCTION

  FUNCTION ExtCCOS(Arg)
  COMPLEX :: ExtCCOS
  COMPLEX, INTENT(IN) :: Arg
    ExtCCOS = CCOS(Arg)
  END FUNCTION

  FUNCTION ExtCEXP(Arg)
  COMPLEX :: ExtCEXP
  COMPLEX, INTENT(IN) :: Arg
    ExtCEXP = CEXP(Arg)
  END FUNCTION

  FUNCTION ExtCLOG(Arg)
  COMPLEX :: ExtCLOG
  COMPLEX, INTENT(IN) :: Arg
    ExtCLOG = CLOG(Arg)
  END FUNCTION

  FUNCTION ExtCONJG(Arg)
  COMPLEX :: ExtCONJG
  COMPLEX, INTENT(IN) :: Arg
    ExtCONJG = CONJG(Arg)
  END FUNCTION

  FUNCTION ExtCOS(Arg)
  REAL :: ExtCOS
  REAL, INTENT(IN) :: Arg
    ExtCOS = COS(Arg)
  END FUNCTION

  FUNCTION ExtCOSH(Arg)
  REAL :: ExtCOSH
  REAL, INTENT(IN) :: Arg
    ExtCOSH = COSH(Arg)
  END FUNCTION

  FUNCTION ExtCSIN(Arg)
  COMPLEX :: ExtCSIN
  COMPLEX, INTENT(IN) :: Arg
    ExtCSIN = CSIN(Arg)
  END FUNCTION

  FUNCTION ExtCSQRT(Arg)
  COMPLEX :: ExtCSQRT
  COMPLEX, INTENT(IN) :: Arg
    ExtCSQRT = CSQRT(Arg)
  END FUNCTION

  FUNCTION Ext(Arg)
  REAL :: Ext
  COMPLEX, INTENT(IN) :: Arg
    Ext = (Arg)
  END FUNCTION


  PROGRAM ExtAssignProcNameIntrin1
  IMPLICIT NONE

  PROCEDURE(ATAN2) :: ExtATAN2
  PROCEDURE(CABS)  :: ExtCABS
  PROCEDURE(CCOS)  :: ExtCCOS
  PROCEDURE(CEXP)  :: ExtCEXP
  PROCEDURE(CLOG)  :: ExtCLOG
  PROCEDURE(CONJG) :: ExtCONJG
  PROCEDURE(COS)   :: ExtCOS
  PROCEDURE(COSH)  :: ExtCOSH
  PROCEDURE(CSIN)  :: ExtCSIN
  PROCEDURE(CSQRT) :: ExtCSQRT

  IF (ABS(ExtATAN2(1.5574077, 1.0) - 1.0) .GT. .00001 ) STOP 11

  IF (ABS(ExtCABS((3.0, 4.0)) - 5.0) .GT. .00001 ) STOP 12

  IF (ABS(ExtCCOS((1.0, .0)) - (0.5403022766,-0.000000E+00)) .GT. .00001 ) STOP 13

  IF (ABS(ExtCEXP((1.0, .0)) - (2.718281746,0.00E+00)) .GT. .00001 ) STOP 14

  IF (ABS(EXP(ExtCLOG((1.0, 1.0))) - (1.0, 1.0)) .GT. .00001 ) STOP 15

  IF (ExtCONJG((2.0, -2.0)) .NE. (2.0, 2.0) )   STOP 16

  IF (ABS(ExtCOS(1.0) - 0.54030231) .GT.  .00001 )   STOP 17

  IF (ABS(ExtCOSH(1.0) - 1.5430806) .GT.  .00001 )   STOP 18

  IF (ABS(ExtCSIN((1.0,1.0)) - (1.298457623,0.6349639297)) .GT. .00001 ) STOP 19

  IF (ABS(ExtCSQRT((1.0,1.0)) - (1.098684072,0.4550898671)) .GT. .00001 ) STOP 20

  END

