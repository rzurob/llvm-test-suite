! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Arg4.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg4.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 27, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 289058 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*   
!* Explicit dummy procedure - Characteristics
!* Non intrinsic elemental proc is illegal
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
      PROCEDURE(IFun), PASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, PASS :: Proc=>ModFun
    END TYPE

    CONTAINS

    PURE FUNCTION ModFun(Arg)
    CLASS(DT), INTENT(IN) :: Arg
    TYPE(DT)    :: ModFun
    PROCEDURE(IFun),  POINTER :: Ptr
      ptr => Arg%ProcPtr ! avoid the violation to pure func
      ModFun = DT(Arg%C, Ptr) 
    END FUNCTION

    PURE FUNCTION IFun(Arg)
    CLASS(DT), INTENT(IN)        :: Arg
    TYPE(DT)                     :: IFun
    PROCEDURE(ModFun),  POINTER  :: Ptr
      ptr => Arg%ProcPtr 
      IFun = DT(Arg%C, Ptr)
    END FUNCTION

  END MODULE

  PURE FUNCTION ExtFun(Arg)
  USE M
  CLASS(DT), INTENT(IN) :: Arg
  TYPE(DT)              :: ExtFun
  PROCEDURE(ModFun),  POINTER  :: Ptr
    ptr => Arg%ProcPtr 
    ExtFun = DT(Arg%C, Ptr)
  END FUNCTION

  PROGRAM Arg4 
  USE M
  IMPLICIT NONE 
  PROCEDURE(IFun) :: ExtFun
  PROCEDURE(IFun), POINTER :: ProcPtr 

  INTERFACE
    PURE FUNCTION IFun1(Arg)
      IMPORT DT
      CLASS(DT), INTENT(IN) :: Arg
      TYPE(DT)              :: IFun1
    END FUNCTION
  END INTERFACE

  CALL IntSub1(ExtFun )

  ProcPtr => ExtFun
  CALL IntSub1( ProcPtr )

  CALL IntSub2( ProcPtr )

  CONTAINS

  SUBROUTINE IntSub1(Arg)
  PROCEDURE(IFun) :: Arg

  TYPE(DT) :: V, U

    V = Arg(DT("123", Arg))
    IF (V%C .NE. "123")                   STOP 11
    IF (.NOT. ASSOCIATED(V%ProcPtr, Arg)) STOP 12

    V%C = "321"
    V%ProcPtr => IFun 
    U = V%Proc()
    IF (U%C .NE. "321")                    STOP 21
    IF (.NOT. ASSOCIATED(U%ProcPtr, IFun)) STOP 22

  END SUBROUTINE

  SUBROUTINE IntSub2(Arg)
  PROCEDURE(IFun1), POINTER :: Arg

  TYPE(DT) :: V, U

    V = Arg(DT("123", Arg))
    IF (V%C .NE. "123")                   STOP 31
    IF (.NOT. ASSOCIATED(V%ProcPtr, Arg)) STOP 32

    V%C = "321"
    V%ProcPtr => IFun 
    U = V%Proc()
    IF (U%C .NE. "321")                    STOP 41
    IF (.NOT. ASSOCIATED(U%ProcPtr, IFun)) STOP 42

  END SUBROUTINE

  END

