! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures2/Arg5.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qck -qnok

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

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C
    END TYPE

  END MODULE

  MODULE M
  USE M0

    TYPE, EXTENDS(Base)  :: DT    ! (4,3)
      PROCEDURE(IFun), NOPASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, NOPASS :: Proc=>ModFun
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    TYPE(DT(4,*)), INTENT(IN) :: Arg
    TYPE(DT(4,3))             ::  ModFun
      ModFun = Arg
    END FUNCTION

    FUNCTION IFun(Arg)
    TYPE(DT(4,*)), INTENT(IN) :: Arg
    TYPE(DT(4,3))             :: IFun
      IFun = Arg
    END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT(4,*)), INTENT(IN) :: Arg
  TYPE(DT(4,3))             ::  ExtFun
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
      TYPE(DT(4,*)), INTENT(IN) :: Arg
      TYPE(DT(4,3))             :: IFun1
    END FUNCTION
  END INTERFACE

  ProcPtr => ExtFun
  CALL IntSub( ProcPtr, ProcPtr, ProcPtr, ExtFun )

  CONTAINS

  SUBROUTINE IntSub(ProcPtr0, ProcPtr1, ProcPtr2, ProcPtr3)
  IMPLICIT TYPE(DT(4,3))(P)

  PROCEDURE(IFun1), POINTER :: ProcPtr0
  PROCEDURE(IFun1), POINTER :: ProcPtr1
  PROCEDURE(IFun1), POINTER :: ProcPtr2
  PROCEDURE(IFun1)          :: ProcPtr3
  TYPE(DT(4,3))             :: V

  IF ( .NOT. ASSOCIATED(ProcPtr0, ExtFun) ) STOP 10
  IF ( .NOT. ASSOCIATED(ProcPtr1, ExtFun) ) STOP 11
  IF ( .NOT. ASSOCIATED(ProcPtr2, ExtFun) ) STOP 12

  V = ProcPtr0(DT(4,3)("321", ProcPtr0))
  IF (V%C .NE. "321") STOP 21
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ProcPtr) ) STOP 22

  V = ProcPtr1(DT(4,3)("121", ExtFun))
  IF (V%C .NE. "121") STOP 31
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ExtFun) ) STOP 32

  V = ProcPtr2(DT(4,3)("331", ProcPtr0))
  IF (V%C .NE. "331") STOP 41
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ExtFun) ) STOP 42

  V = ProcPtr3(DT(4,3)("122", ExtFun))
  IF (V%C .NE. "122") STOP 51
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ExtFun) ) STOP 52

  END SUBROUTINE

  END

