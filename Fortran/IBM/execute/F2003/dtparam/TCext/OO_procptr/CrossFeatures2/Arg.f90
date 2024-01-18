! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures2/Arg.f
! opt variations: -qnock

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

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C
    END TYPE

  END MODULE

  MODULE M
  USE M0

    TYPE, EXTENDS(Base)  :: DT    ! (1,3)
      PROCEDURE(IFun), PASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, PASS :: Proc=>ModFun
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT(1,*)) :: Arg
    TYPE(Base(1,3)) ::  ModFun
      ModFun = Arg%Base
    END FUNCTION

    FUNCTION IFun(Arg)
    CLASS(DT(1,*)) :: Arg
    TYPE(Base(1,3)) ::  IFun
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
  IMPLICIT TYPE(DT(1,3))(P)
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
  TYPE(Base(1,3)) :: V, W
  TYPE(DT(1,3))   :: U

  V = Ptr(DT(1,3)(Base=Base(1,3)("123"), ProcPtr=Ptr))
  IF ( V%C .NE. "123" ) STOP 12

  U = DT(1,3)(C="123",ProcPtr=RetPtr(Ptr))
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun) ) STOP 32

  W = U%Proc()
  IF ( W%C  .NE. "123" ) STOP 33

  END SUBROUTINE

  END

