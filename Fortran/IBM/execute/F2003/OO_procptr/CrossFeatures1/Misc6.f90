! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp Misc6.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc6.f
!*
!*  DATE                       : May. 31, 2005
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
!* Implicit interface on Arg1
!* (Ref-304507)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  CONTAINS

  FUNCTION FChar(Arg)
  CHARACTER :: Arg
  CHARACTER :: FChar
    FChar = Arg
  END FUNCTION

  END MODULE

  PROGRAM Misc6
  USE M
  CHARACTER(5) :: C
  PROCEDURE(), POINTER :: ProcPtr
  CHARACTER            :: ProcPtr

  ProcPtr => FChar
  IF ( ProcPtr("1")                    .NE. "1" ) STOP 11
  IF ( ProcPtr(IntFun1 (FChar, "2"))   .NE. "2" ) STOP 12
  IF ( ProcPtr(IntFun1 (ProcPtr, "3")) .NE. "3" ) STOP 13
  IF ( ProcPtr(IntFun2 (ProcPtr, "4")) .NE. "4" ) STOP 14
  IF ( IntFun1 (ProcPtr, "5")          .NE. "5" ) STOP 15
  IF ( IntFun2 (ProcPtr, "6")          .NE. "6" ) STOP 16

  CONTAINS

  FUNCTION IntFun1(Arg1, Arg2)
  CHARACTER(*) :: Arg1, Arg2
  CHARACTER(LEN(Arg2)) :: IntFun1
    IntFun1 = Arg1(Arg2)
  END FUNCTION

  FUNCTION IntFun2(Arg1, Arg2)
  CHARACTER(*) :: Arg2
  CHARACTER(LEN(Arg2)) :: IntFun2
! CHARACTER(LEN(Arg2)) :: Arg1
  CHARACTER(1)         :: Arg1
  PROCEDURE()          :: Arg1
    IntFun2 = Arg1(Arg2)
  END FUNCTION

  END


