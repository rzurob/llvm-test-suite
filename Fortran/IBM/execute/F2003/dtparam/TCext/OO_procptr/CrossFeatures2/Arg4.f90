! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures2/Arg4.f
! opt variations: -qck -qnok

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
!*
!*  TEST CASE NAME             : Arg4.f
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
!* Explicit dummy procedure - Characteristics
!* Non intrinsic elemental proc is illegal
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M0

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C
    END TYPE

  END MODULE

  MODULE M
  USE M0

    TYPE, EXTENDS(Base)  :: DT    ! (4,3)
      PROCEDURE(IFun), PASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, PASS :: Proc=>ModFun
    END TYPE

    CONTAINS

    PURE FUNCTION ModFun(Arg)
    CLASS(DT(4,*)), INTENT(IN) :: Arg
    TYPE(DT(4,3))    :: ModFun
    PROCEDURE(IFun),  POINTER :: Ptr
      ptr => Arg%ProcPtr ! avoid the violation to pure func
      ModFun = DT(4,3)(Arg%C, Ptr)
    END FUNCTION

    PURE FUNCTION IFun(Arg)
    CLASS(DT(4,*)), INTENT(IN)        :: Arg
    TYPE(DT(4,3))                     :: IFun
    PROCEDURE(ModFun),  POINTER  :: Ptr
      ptr => Arg%ProcPtr
      IFun = DT(4,3)(Arg%C, Ptr)
    END FUNCTION

  END MODULE

  PURE FUNCTION ExtFun(Arg)
  USE M
  CLASS(DT(4,*)), INTENT(IN) :: Arg
  TYPE(DT(4,3))              :: ExtFun
  PROCEDURE(ModFun),  POINTER  :: Ptr
    ptr => Arg%ProcPtr
    ExtFun = DT(4,3)(Arg%C, Ptr)
  END FUNCTION

  PROGRAM Arg4
  USE M
  IMPLICIT NONE
  PROCEDURE(IFun) :: ExtFun
  PROCEDURE(IFun), POINTER :: ProcPtr

  INTERFACE
    PURE FUNCTION IFun1(Arg)
      IMPORT DT
      CLASS(DT(4,*)), INTENT(IN) :: Arg
      TYPE(DT(4,3))              :: IFun1
    END FUNCTION
  END INTERFACE

  CALL IntSub1(ExtFun )

  ProcPtr => ExtFun
  CALL IntSub1( ProcPtr )

  CALL IntSub2( ProcPtr )

  CONTAINS

  SUBROUTINE IntSub1(Arg)
  PROCEDURE(IFun) :: Arg

  TYPE(DT(4,3)) :: V, U

    V = Arg(DT(4,3)("123", Arg))
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

  TYPE(DT(4,3)) :: V, U

    V = Arg(DT(4,3)("123", Arg))
    IF (V%C .NE. "123")                   STOP 31
    IF (.NOT. ASSOCIATED(V%ProcPtr, Arg)) STOP 32

    V%C = "321"
    V%ProcPtr => IFun
    U = V%Proc()
    IF (U%C .NE. "321")                    STOP 41
    IF (.NOT. ASSOCIATED(U%ProcPtr, IFun)) STOP 42

  END SUBROUTINE

  END

