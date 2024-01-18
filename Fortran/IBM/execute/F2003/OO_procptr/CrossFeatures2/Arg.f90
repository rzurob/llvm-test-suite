! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Arg.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg.f
!*
!*  DATE                       : Jun. 25 2005
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
!*  Dummy is a procedure pointer - procedure pointer/function/Null
!*  (314722)
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
      PROCEDURE(IFun), PASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, PASS :: Proc=>ModFun
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT) :: Arg
    TYPE(Base) ::  ModFun
      ModFun = Arg%Base
    END FUNCTION

    FUNCTION IFun(Arg)
    CLASS(DT) :: Arg
    TYPE(Base) ::  IFun
      IFun = Arg%Base
    END FUNCTION

  END MODULE

  FUNCTION RetPtr(Fun)
  USE M
  PROCEDURE(IFun)          :: Fun
  PROCEDURE(IFun), POINTER :: RetPtr
    RetPtr => Fun
  END FUNCTION

  PROGRAM Arg
  USE M
  IMPLICIT TYPE(DT)(P)
  PROCEDURE(IFun), POINTER :: ProcPtr

  INTERFACE
    FUNCTION RetPtr(Fun)
    IMPORT IFun
      PROCEDURE(IFun)          :: Fun
      PROCEDURE(IFun), POINTER :: RetPtr
    END FUNCTION
  END INTERFACE

  ProcPtr => ModFun
  CALL IntSub(ProcPtr)
  CALL IntSub(RetPtr(ProcPtr))

  CONTAINS

  SUBROUTINE IntSub(Ptr)
  PROCEDURE(IFun), POINTER :: Ptr
  TYPE(Base) :: V, W
  TYPE(DT)   :: U

  V = Ptr(DT(Base=Base("123"), ProcPtr=Ptr))
  IF ( V%C .NE. "123" ) STOP 12

  U = DT(C="123",ProcPtr=RetPtr(Ptr))
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun) ) STOP 32

  W = U%Proc()
  IF ( W%C  .NE. "123" ) STOP 33

  END SUBROUTINE

  END

