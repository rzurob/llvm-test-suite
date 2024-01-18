! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  Misc8.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  Misc8.f
!*
!*  DATE                       : Jun. 08, 2005
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
!*  Calling proc ptr in default IO
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    INTEGER :: I=1
  END TYPE

  INTEGER :: Count = 0

  CONTAINS

  FUNCTION ModFun()
  TYPE(DT) :: ModFun
    ModFun%I = -1
    Count = Count + 1
  END FUNCTION

  END MODULE

  PROGRAM Misc8
  USE M
  IMPLICIT TYPE(DT)(P)

  PROCEDURE(),         POINTER  :: ProcPtr1
  PROCEDURE(TYPE(DT)), POINTER  :: ProcPtr2
  PROCEDURE(ModFun),   POINTER  :: ProcPtr3

  ProcPtr1 => ModFun
  ProcPtr2 => ModFun
  ProcPtr3 => ModFun

  PRINT *, ProcPtr1()
  PRINT *, ProcPtr2()
  PRINT *, ProcPtr3()

  IF (Count .NE. 3) STOP 11

  END


