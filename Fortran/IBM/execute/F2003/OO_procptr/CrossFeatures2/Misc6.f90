! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Misc6.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc6.f
!*
!*  DATE                       : May. 26, 2005
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
!*  Procedure pointer - access spec
!*  (315097->315733)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    INTEGER :: Id
    PROCEDURE(ModFun), PASS, POINTER, PRIVATE :: ProcPtr1=>NULL()
    PROCEDURE(ModFun), PASS, POINTER, PUBLIC  :: ProcPtr2
  END TYPE

  PROCEDURE(ModFun), POINTER, PRIVATE   :: ProcPtr1
  PROCEDURE(ProcPtr1), POINTER, PUBLIC  :: ProcPtr2
  TYPE(DT), SAVE                        :: V

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT) :: Arg
  TYPE(DT)  :: ModFun
    ModFun = Arg
  END FUNCTION

  SUBROUTINE Check()

    ProcPtr1 => ModFun
    V = ProcPtr1(DT(-1, ModFun, ModFun))
    IF ( V%ID .NE. -1) STOP 11
    IF ( .NOT. ASSOCIATED(V%ProcPtr1, ModFun) ) STOP 12
    IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) STOP 13

    V = DT(0, NULL(), NULL())
    ProcPtr2 => ModFun
    V = ProcPtr2(DT(-2, ModFun, ModFun))
    IF ( V%ID .NE. -2) STOP 21
    IF ( .NOT. ASSOCIATED(V%ProcPtr1, ModFun) ) STOP 22
    IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) STOP 23

    V = DT(0, NULL(), NULL())
    V%ID = -3
    V%ProcPtr1 => ModFun
    V%ProcPtr2 => ModFun
    V = V%ProcPtr1()
    IF ( V%ID .NE. -3) STOP 31
    IF ( .NOT. ASSOCIATED(V%ProcPtr1, ModFun) ) STOP 32
    IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) STOP 33

    V = DT(-4, NULL(), NULL())
    V%ProcPtr1 => ModFun
    V%ProcPtr2 => ModFun
    V = V%ProcPtr2()
    IF ( V%ID .NE. -4) STOP 41
    IF ( .NOT. ASSOCIATED(V%ProcPtr1, ModFun) ) STOP 42
    IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) STOP 43

  END SUBROUTINE

  END MODULE

  PROGRAM Misc6
  USE M
  IMPLICIT NONE

  CALL Check()

  V = DT(0, ProcPtr2=NULL())
  ProcPtr2 => ModFun
  V = ProcPtr2(DT(-5, ProcPtr2=ModFun))
  IF ( V%ID .NE. -5) STOP 51
  IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) STOP 53

  V = DT(-6, ProcPtr2=NULL())
  V%ProcPtr2 => ModFun
  V = V%ProcPtr2()
  IF ( V%ID .NE. -6) STOP 61
  IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) STOP 63

  END

