! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignProcNameIntrin4.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameIntrin3.f
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
!*  (315324/316469/316083/330287)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  INTERFACE
    FUNCTION ExtDTANH(Arg)
      DOUBLE PRECISION :: ExtDTANH
      DOUBLE PRECISION, INTENT(IN) :: Arg
    END FUNCTION

    FUNCTION ExtEXP(Arg)
      REAL :: ExtExp
      REAL, INTENT(IN) :: Arg
    END FUNCTION

    FUNCTION ExtIABS(Arg)
      INTEGER :: ExtIABS
      INTEGER, INTENT(IN) :: Arg
    END FUNCTION

    FUNCTION ExtIDIM(Arg1, Arg2)
      INTEGER :: ExtIDIM
      INTEGER, INTENT(IN) :: Arg1, Arg2
    END FUNCTION

    FUNCTION ExtIDNINT(Arg)
      INTEGER :: ExtIDNINT
      DOUBLE PRECISION, INTENT(IN) :: Arg
    END FUNCTION

!   FUNCTION ExtINDEX(Arg1, Arg2, Arg3, Arg4)  ! As the KIND arg of INDEX is not a real Argument. it presents a specific interface
    FUNCTION ExtINDEX(Arg1, Arg2, Arg3)
      INTEGER :: ExtINDEX
      CHARACTER(*), INTENT(IN) :: Arg1, Arg2
      LOGICAL, OPTIONAL, INTENT(IN) :: Arg3
!     INTEGER, OPTIONAL, INTENT(IN) :: Arg4
    END FUNCTION

    FUNCTION ExtISIGN(Arg1, Arg2)
      INTEGER :: ExtISIGN
      INTEGER, INTENT(IN) :: Arg1, Arg2
    END FUNCTION

!   FUNCTION ExtLEN(Arg1, Arg2)
!     INTEGER :: ExtLEN
!     CHARACTER(*), INTENT(IN) :: Arg1
!     INTEGER, OPTIONAL, INTENT(IN) :: Arg2
!   END FUNCTION

    FUNCTION ExtMOD(Arg1, Arg2)
      INTEGER :: ExtMOD
      INTEGER, INTENT(IN) :: Arg1, Arg2
    END FUNCTION

!   FUNCTION ExtNINT(Arg1, Arg2)
    FUNCTION ExtNINT(Arg1)
      INTEGER :: ExtNINT
      REAL, INTENT(IN) :: Arg1
!     INTEGER, OPTIONAL, INTENT(IN) :: Arg2
    END FUNCTION

  END INTERFACE


  TYPE :: DT
    PROCEDURE(ExtDTANH),  POINTER, NOPASS :: PtrDTANH
    PROCEDURE(ExtEXP),    POINTER, NOPASS :: PtrEXP
    PROCEDURE(ExtIABS),   POINTER, NOPASS :: PtrIABS
    PROCEDURE(ExtIDIM),   POINTER, NOPASS :: PtrIDIM
    PROCEDURE(ExtIDNINT), POINTER, NOPASS :: PtrIDNINT
    PROCEDURE(ExtINDEX),  POINTER, NOPASS :: PtrINDEX
    PROCEDURE(ExtISIGN),  POINTER, NOPASS :: PtrISIGN
!   PROCEDURE(ExtLEN),    POINTER, NOPASS :: PtrLEN
    PROCEDURE(LEN),       POINTER, NOPASS :: PtrLEN
    PROCEDURE(ExtMOD),    POINTER, NOPASS :: PtrMOD
    PROCEDURE(ExtNINT),   POINTER, NOPASS :: PtrNINT
  END TYPE

  TYPE (DT) :: U

  END MODULE

  PROGRAM PtrAssignProcNameIntrin4
  USE M, V=>U
  IMPLICIT NONE

  V%PtrDTANH => DTANH
  IF (ABS(V%PtrDTANH(1.0D0) - 0.76159416D0) .GT. .00001 ) STOP 11

  V%PtrEXP => EXP
  IF (ABS(V%PtrEXP(1.0) - 2.7182818 ) .GT. .00001 ) STOP 12

  V%PtrIABS => IABS
  IF (V%PtrIABS(-3) .NE.  3 ) STOP 13

  V%PtrIDIM => IDIM
  IF (V%PtrIDIM(-3, 2) .NE. 0 ) STOP 14

  V%PtrIDNINT => IDNINT
  IF (V%PtrIDNINT(2.783D0) .NE. 3) STOP 15

  V%PtrINDEX => INDEX
  IF (V%PtrINDEX('FORTRAN', 'R') .NE. 3 ) STOP 16

  V%PtrISIGN => ISIGN
  IF (V%PtrISIGN(-3, 2) .NE. 3 ) STOP 17

  V%PtrLEN => LEN
  IF (V%PtrLEN('-3') .NE. 2 ) STOP 18

  V%PtrMOD => MOD
  IF (V%PtrMOD(-8, -5) .NE. -3 ) STOP 19

  V%PtrNINT => NINT
  IF (V%PtrNINT(2.783) .NE. 3 ) STOP 20

  END

