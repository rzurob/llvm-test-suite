! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/OO_procptr/CrossFeatures2/Arg1.f
! opt variations: -qnock -qnodeferredlp

! *********************************************************************
!*  ===================================================================
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
!*  Dummy argument is a procedure pointer - procedure pointer return
!*  ()
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
      PROCEDURE(IFun), NoPASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, NoPASS :: Proc=>ModFun
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(Base(1,*)) :: Arg
    CLASS(Base(1,:)), POINTER ::  ModFun
      ALLOCATE(ModFun, SOURCE=Arg)
    END FUNCTION

    FUNCTION IFun(Arg)
    CLASS(Base(1,*)) :: Arg
    CLASS(Base(1,:)), POINTER ::  IFun
      ALLOCATE(IFun, SOURCE=Arg)
    END FUNCTION

  END MODULE

  FUNCTION RetPtr(Fun)
  USE M
  PROCEDURE(IFun)          :: Fun
  PROCEDURE(IFun), POINTER :: RetPtr
    RetPtr => Fun
  END FUNCTION

  PROGRAM Arg1
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

  CALL IntSub(Modfun)

  ProcPtr => ModFun
  CALL IntSub(ProcPtr)

  CALL IntSub(RetPtr(ProcPtr))

  CONTAINS

  SUBROUTINE IntSub(Ptr)
  PROCEDURE(IFun) :: Ptr
  TYPE(Base(1,3)) :: V
  TYPE(DT(1,3))   :: U, W

  V = Ptr(Base(1,3)("123"))
  IF ( V%C .NE. "123" ) STOP 12

  U = DT(1,3)(C="123",ProcPtr=RetPtr(Ptr))
  IF ( .NOT. ASSOCIATED(U%ProcPtr, Ptr) ) STOP 32

  SELECT TYPE (As => U%ProcPtr(U))
  TYPE IS (DT(1,*))
    W = As
    IF ( W%C  .NE. "123" ) STOP 33
    IF ( .NOT. ASSOCIATED(W%ProcPtr, Ptr) ) STOP 34
  CLASS  DEFAULT
    STOP 35
  END SELECT

  SELECT TYPE (As => U%Proc(U))
  TYPE IS (DT(1,*))
    W = As
    IF ( W%C  .NE. "123" ) STOP 43
    IF ( .NOT. ASSOCIATED(W%ProcPtr, Ptr) ) STOP 44
  CLASS  DEFAULT
    STOP 45
  END SELECT
  END SUBROUTINE

  END

