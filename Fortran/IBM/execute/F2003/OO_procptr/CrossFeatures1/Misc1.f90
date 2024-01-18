! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Misc1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc1.f
!*
!*  DATE                       : May. 20, 2005
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
!*  The intrinsic rouitne
!*
!* (304080 - wait for 11.1) -> this scenario has been covered in
!* CrossFeatures2 for 11.1. Change the TC to the scenario of entry.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  FUNCTION Abs0(Arg)
  REAL :: Arg, Abs0, Abs1, Abs2
    Abs0 = Abs(Arg)
    RETURN
  ENTRY Abs1(Arg)
    Abs1 = Abs(Arg)
    RETURN
  ENTRY Abs2(Arg)
    Abs2 = Abs(Arg)
    RETURN
  END FUNCTION

  MODULE M
  CONTAINS
    FUNCTION Abs3(Arg)
    REAL :: Arg, Abs3, Abs4
      Abs3 = Abs(Arg)
      RETURN
    ENTRY Abs4(Arg)
      Abs4 = Abs(Arg)
      RETURN
    END FUNCTION
  END MODULE

  PROGRAM Misc1
  USE M

  INTERFACE
    FUNCTION RToR(Arg)
      REAL :: RToR, Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(REAL), POINTER :: ProcPtr1
  PROCEDURE(RToR), POINTER :: ProcPtr2

  INTERFACE
    FUNCTION Abs0(Arg)
      REAL :: Arg, Abs0
    END FUNCTION

    FUNCTION Abs1(Arg)
      REAL :: Arg, Abs1
    END FUNCTION

    FUNCTION Abs2(Arg)
      REAL :: Arg, Abs2
    END FUNCTION

  END INTERFACE

  ProcPtr1 => ABS0
  PRINT *, ProcPtr1(-2.0)
  PRINT *, ABS(-2.0)
  IF ( ProcPtr1(-2.0) .NE. ABS(-2.0)) STOP 11

  ProcPtr1 => ABS1
  PRINT *, ProcPtr1(-2.0)
  PRINT *, ABS(-2.0)
  IF ( ProcPtr1(-2.0) .NE. ABS(-2.0)) STOP 12

  ProcPtr2 => ABS2
  PRINT *, ProcPtr2(-2.0)
  PRINT *, ABS(-2.0)
  IF ( ProcPtr2(-2.0) .NE. ABS(-2.0)) STOP 13

  ProcPtr2 => ABS3
  PRINT *, ProcPtr2(-2.0)
  PRINT *, ABS(-2.0)
  IF ( ProcPtr2(-2.0) .NE. ABS(-2.0)) STOP 14

  ProcPtr2 => ABS4
  PRINT *, ProcPtr2(-2.0)
  PRINT *, ABS(-2.0)
  IF ( ProcPtr2(-2.0) .NE. ABS(-2.0)) STOP 15

  END


!  The following is the orginal scenario

! PROGRAM Misc1

! INTERFACE
!   FUNCTION RToR(Arg)
!     REAL :: RToR, Arg
!   END FUNCTION
! END INTERFACE

! PROCEDURE(REAL), POINTER :: ProcPtr1
! PROCEDURE(RToR), POINTER :: ProcPtr2

! INTRINSIC  ABS

! ProcPtr1 => ABS
! PRINT *, ProcPtr1(-2.0)
! PRINT *, ABS(-2.0)
! IF ( ProcPtr1(-2.0) .NE. ABS(-2.0)) STOP 11

! ProcPtr2 => ABS
! PRINT *, ProcPtr2(-2.0)
! PRINT *, ABS(-2.0)
! IF ( ProcPt2(-2.0) .NE. ABS(-2.0)) STOP 12

! END

