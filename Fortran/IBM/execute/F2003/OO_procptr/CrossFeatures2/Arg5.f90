! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Arg5.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg5.f
!*
!*  DATE                       : Jun. 27, 2005
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
!*  Argument association - Implicit interface
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M0

    TYPE :: Base
      CHARACTER(3) :: C
    END TYPE

  END MODULE

  MODULE M
  USE M0

    TYPE, EXTENDS(Base)  :: DT
      PROCEDURE(IFun), NOPASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, NOPASS :: Proc=>ModFun
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    TYPE(DT), INTENT(IN) :: Arg
    TYPE(DT)             ::  ModFun
      ModFun = Arg
    END FUNCTION

    FUNCTION IFun(Arg)
    TYPE(DT), INTENT(IN) :: Arg
    TYPE(DT)             :: IFun
      IFun = Arg
    END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT), INTENT(IN) :: Arg
  TYPE(DT)             ::  ExtFun
    ExtFun = Arg
  END FUNCTION

  PROGRAM Arg5
  USE M
  IMPLICIT NONE
  PROCEDURE(IFun) :: ExtFun
  PROCEDURE(IFun), POINTER :: ProcPtr

  INTERFACE
    FUNCTION IFun1(Arg)
      IMPORT DT
      TYPE(DT), INTENT(IN) :: Arg
      TYPE(DT)             :: IFun1
    END FUNCTION
  END INTERFACE

  ProcPtr => ExtFun
  CALL IntSub( ProcPtr, ProcPtr, ProcPtr, ExtFun )

  CONTAINS

  SUBROUTINE IntSub(ProcPtr0, ProcPtr1, ProcPtr2, ProcPtr3)
  IMPLICIT TYPE(DT)(P)

  PROCEDURE(IFun1),      POINTER :: ProcPtr0
  PROCEDURE(TYPE(DT)),   POINTER :: ProcPtr1
  PROCEDURE(),           POINTER :: ProcPtr2
  PROCEDURE()                    :: ProcPtr3
  TYPE(DT)                       :: V

  IF ( .NOT. ASSOCIATED(ProcPtr0, ExtFun) ) STOP 10
  IF ( .NOT. ASSOCIATED(ProcPtr1, ExtFun) ) STOP 11
  IF ( .NOT. ASSOCIATED(ProcPtr2, ExtFun) ) STOP 12

  V = ProcPtr0(DT("321", ProcPtr0))
  IF (V%C .NE. "321") STOP 21
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ProcPtr) ) STOP 22

  V = ProcPtr1(DT("121", ExtFun))
  IF (V%C .NE. "121") STOP 31
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ExtFun) ) STOP 32

  V = ProcPtr2(DT("331", ProcPtr0))
  IF (V%C .NE. "331") STOP 41
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ExtFun) ) STOP 42

  V = ProcPtr3(DT("122", ExtFun))
  IF (V%C .NE. "122") STOP 51
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ExtFun) ) STOP 52

  END SUBROUTINE

  END

