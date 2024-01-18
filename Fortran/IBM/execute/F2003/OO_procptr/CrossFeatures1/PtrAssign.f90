! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssign.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssign.f
!*
!*  DATE                       : Mar. 11, 2005
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
!*  Procedure pointer
!*
!*  (304373)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    PROCEDURE (ModFun), POINTER :: ProcPtr => NULL()
  CONTAINS
    FUNCTION ModFun(Arg)
    INTEGER(1) :: ModFun, Arg
      ModFun = Arg
    END FUNCTION
  END MODULE

  PROGRAM  PtrAssign
  USE M, LProcPtr => ProcPtr, LModFun => ModFun
  IMPLICIT NONE
  PROCEDURE (LModFun), POINTER :: LProcPtr1

    LProcPtr => LModFun
    IF ( LProcPtr(10_1) .NE. 10_1 ) STOP 11

    LProcPtr1 => LModFun
    IF ( LProcPtr1(1_1) .NE. 1_1 )  STOP 12

    IF ( LProcPtr(LProcPtr(1_1)) .NE. 1_1 )    STOP 13
    IF ( LProcPtr1(LProcPtr1(2_1)) .NE. 2_1 )  STOP 14

    IF ( LProcPtr(LProcPtr1(127_1)) .NE. 127_1 )    STOP 15
    IF ( LProcPtr1(LProcPtr(-128_1)) .NE. -128_1 )  STOP 16

  END
