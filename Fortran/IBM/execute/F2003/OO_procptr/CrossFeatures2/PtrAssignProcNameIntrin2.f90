! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: ExtAssignProcNameIntrin2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ExtAssignProcNameIntrin2.f
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
!*  Intrinsics -- TC changed due to C1215
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  FUNCTION ExtDABS(Arg)
  DOUBLE PRECISION :: ExtDABS
  DOUBLE PRECISION, INTENT(IN) :: Arg
    ExtDABS = DABS(Arg)
  END FUNCTION

  FUNCTION ExtDACOS(Arg)
  DOUBLE PRECISION :: ExtDACOS
  DOUBLE PRECISION, INTENT(IN) :: Arg
    ExtDACOS = DACOS(Arg)
  END FUNCTION

  FUNCTION ExtDATAN(Arg)
  DOUBLE PRECISION :: ExtDATAN
  DOUBLE PRECISION, INTENT(IN) :: Arg
    ExtDATAN = DATAN(Arg)
  END FUNCTION

  FUNCTION ExtDATAN2(Arg1, Arg2)
  DOUBLE PRECISION :: ExtDATAN2
  DOUBLE PRECISION, INTENT(IN) :: Arg1, Arg2
    ExtDATAN2 = DATAN2(Arg1, Arg2)
  END FUNCTION

  FUNCTION ExtDCOS(Arg)
  DOUBLE PRECISION :: ExtDCOS
  DOUBLE PRECISION, INTENT(IN) :: Arg
    ExtDCOS = DCOS(Arg)
  END FUNCTION

  FUNCTION ExtDCOSH(Arg)
  DOUBLE PRECISION :: ExtDCOSH
  DOUBLE PRECISION, INTENT(IN) :: Arg
    ExtDCOSH = DCOSH(Arg)
  END FUNCTION

  FUNCTION ExtDDIM(Arg1, Arg2)
  DOUBLE PRECISION :: ExtDDIM
  DOUBLE PRECISION, INTENT(IN) :: Arg1, Arg2
    ExtDDIM = DDIM(Arg1, Arg2)
  END FUNCTION

  FUNCTION ExtDEXP(Arg)
  DOUBLE PRECISION :: ExtDEXP
  DOUBLE PRECISION, INTENT(IN) :: Arg
    ExtDEXP = DEXP(Arg)
  END FUNCTION

  FUNCTION ExtDIM(Arg1, Arg2)
  DOUBLE PRECISION :: ExtDIM
  REAL, INTENT(IN) :: Arg1, Arg2
    ExtDIM = DIM(Arg1, Arg2)
  END FUNCTION

  FUNCTION ExtDINT(Arg)
  DOUBLE PRECISION :: ExtDINT
  DOUBLE PRECISION, INTENT(IN) :: Arg
    ExtDINT = DINT(Arg)
  END FUNCTION


  PROGRAM ExtAssignProcNameIntrin2
  IMPLICIT NONE

  PROCEDURE(DABS)   :: ExtDABS
  PROCEDURE(DACOS)  :: ExtDACOS
  PROCEDURE(DATAN)  :: ExtDATAN
  PROCEDURE(DATAN2) :: ExtDATAN2
  PROCEDURE(DCOS)   :: ExtDCOS
  PROCEDURE(DCOSH)  :: ExtDCOSH
  PROCEDURE(DDIM)   :: ExtDDIM
  PROCEDURE(DEXP)   :: ExtDExp
  PROCEDURE(DIM)    :: ExtDIM

  PROCEDURE(DINT)   :: ExtDINT

  IF (ExtDABS(-2.D0) .NE. 2.D0 ) STOP 11

  IF (ABS(ExtDACOS(0.54030231D0) - 1.0D0 ) .GT. .00001 ) STOP 12

  IF (ABS(ExtDATAN(1.5574077D0) - 1.D0) .GT. .00001 ) STOP 13

  IF (ABS(ExtDATAN2(1.5574077D0, 1.0D0) - 1.D0) .GT. .00001 ) STOP 14

  IF (ABS(ExtDCOS(1.0D0) - 0.54030231D0 ) .GT. .00001 ) STOP 15

  IF (ABS(ExtDCOSH(1.0D0) - 1.5430806D0 ) .GT. .00001 ) STOP 16

  IF (ExtDDIM(-3.0D0, 2.0D0) .NE. 0.0D0 ) STOP 17

  IF (ABS(ExtDEXP(1.0D0) - 2.7182818D0) .GT. .00001 ) STOP 18

  IF (ExtDDIM(-3.0_8, 2.0_8) .NE. 0.0 ) STOP 19

  IF (ExtDINT(-2.783D0) .NE. -2.0D0 )   STOP 20

  END

