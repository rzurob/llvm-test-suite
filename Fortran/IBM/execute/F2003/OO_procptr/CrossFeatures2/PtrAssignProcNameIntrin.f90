! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignProcNameIntrin.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameIntrin.f
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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM PtrAssignProcNameIntrin
  IMPLICIT NONE

  INTERFACE
    REAL FUNCTION IABS(Arg)
      REAL, INTENT(IN) ::  Arg
    END FUNCTION

    REAL FUNCTION IACOS(Arg)
      REAL, INTENT(IN) ::  Arg
    END FUNCTION

    REAL FUNCTION IAIMAG(Arg)
      COMPLEX, INTENT(IN) :: Arg
    END FUNCTION

!   REAL FUNCTION IAINT(Arg1, Arg2)
!     INTEGER, OPTIONAL, INTENT(IN) ::  Arg2
    REAL FUNCTION IAINT(Arg)
      REAL, INTENT(IN) ::  Arg
    END FUNCTION

    REAL FUNCTION IALOG(Arg)
      REAL, INTENT(IN) ::  Arg
    END FUNCTION

    REAL FUNCTION IALOG10(Arg)
      REAL, INTENT(IN) ::  Arg
    END FUNCTION

    REAL FUNCTION IAMOD(Arg1, Arg2)
      REAL, INTENT(IN) ::  Arg1, Arg2
    END FUNCTION

!   REAL FUNCTION IANINT(Arg1, Arg2)
!     INTEGER, OPTIONAL, INTENT(IN) ::  Arg2
    REAL FUNCTION IANINT(Arg)
      REAL, INTENT(IN) ::  Arg
    END FUNCTION

    REAL FUNCTION IASIN(Arg)
      REAL, INTENT(IN) ::  Arg
    END FUNCTION

    REAL FUNCTION IATAN(Arg)
      REAL, INTENT(IN) ::  Arg
    END FUNCTION

  END INTERFACE

  PROCEDURE(IABS),   POINTER :: PtrABS
  PROCEDURE(IACOS),  POINTER :: PtrACOS
  PROCEDURE(IAIMAG), POINTER :: PtrAIMAG
  PROCEDURE(IAINT),  POINTER :: PtrAINT
  PROCEDURE(IALOG),  POINTER :: PtrALOG
  PROCEDURE(IALOG10),POINTER :: PtrLOG10
  PROCEDURE(IAMOD),  POINTER :: PtrAMOD
  PROCEDURE(IANINT), POINTER :: PtrANINT
  PROCEDURE(IASIN),  POINTER :: PtrASIN
  PROCEDURE(IATAN),  POINTER :: PtrATAN

  PtrABS => ABS
  IF (ABS(PtrABS(-1.0) - 1.0) .GT. .00001 ) STOP 11

  PtrACOS => ACOS
  IF (PtrABS(PtrACOS(0.54030231) - 1.0) .GT. .00001 ) STOP 12

  PtrAIMAG => AIMAG
  IF (PtrAIMAG((2.0, -1.0)) .NE. -1.0 )   STOP 13

  PtrAINT => AINT
  IF (PtrAINT(-2.783) .NE. -2.0 )   STOP 14

  PtrALOG => ALOG
  IF (PtrALOG(1.0) .NE. 0.0 ) STOP 15

  PtrLOG10 => ALOG10
  IF (PtrLOG10(1.0) .NE. 0.0 ) STOP 16

  PtrAMOD => AMOD
  IF (PtrAMOD(3.0, 2.0) .NE. 1.0 ) STOP 17

  PtrANINT => ANINT
  IF (PtrANINT(2.783) .NE. 3.0 ) STOP 18

  PtrASIN => ASIN
  IF (PtrABS(PtrASIN(0.84147098)-1.0) .GT. .00001 ) STOP 19

  PtrATAN => ATAN
  IF (PtrABS(PtrATAN(1.5574077)-1.0) .GT. .00001 ) STOP 20

  END

