! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Arg6.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg6.f
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
!*  Argument association - Implicit interface  of subroutine
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
      PROCEDURE(), NOPASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, NOPASS :: Proc=>ModSub
    END TYPE

    CONTAINS

    SUBROUTINE ModSub(Arg1, Arg2)
    TYPE(DT), INTENT(IN)  :: Arg2
    TYPE(DT), INTENT(OUT) :: Arg1
      Arg1 = Arg2
    END SUBROUTINE

    SUBROUTINE ISub(Arg1, Arg2)
    TYPE(DT), INTENT(IN)  :: Arg2
    TYPE(DT), INTENT(OUT) :: Arg1
      Arg1 = Arg2
    END SUBROUTINE

  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(DT), INTENT(IN)  :: Arg2
  TYPE(DT), INTENT(OUT) :: Arg1
    Arg1 = Arg2
  END SUBROUTINE

  PROGRAM Arg6
  USE M
  IMPLICIT NONE
  PROCEDURE(Isub) :: ExtSub
  PROCEDURE(ISub), POINTER :: ProcPtr

  ProcPtr => ExtSub
  CALL IntSub( ProcPtr, ProcPtr )

  CALL IntSub( IntFun1(ExtSub), IntFun1(ProcPtr))
  CALL IntSub( ProcPtr, IntFun2(ExtSub))

  CONTAINS

  FUNCTION IntFun1(Proc)
  PROCEDURE(ISub) :: Proc
  PROCEDURE(ISub), POINTER :: IntFun1
    IntFun1 => Proc
  END FUNCTION

  FUNCTION IntFun2(Proc)
  PROCEDURE() :: Proc
  PROCEDURE(), POINTER :: IntFun2
    IntFun2 => Proc
  END FUNCTION


  SUBROUTINE IntSub(ProcPtr0, ProcPtr1)
  IMPLICIT TYPE(Base)(P)

  PROCEDURE(ISub),       POINTER :: ProcPtr0
  PROCEDURE(),           POINTER :: ProcPtr1
  TYPE(DT)                       :: V

  IF ( .NOT. ASSOCIATED(ProcPtr0, ExtSub) ) STOP 10
  IF ( .NOT. ASSOCIATED(ProcPtr1, ExtSub) ) STOP 11

  CALL ProcPtr0(V, DT("321", ProcPtr1))
  IF (V%C .NE. "321") STOP 15
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ExtSub)) STOP 16

  CALL ProcPtr1(V, DT("321", ProcPtr1))
  IF (V%C .NE. "321") STOP 17
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ExtSub)) STOP 18

  END SUBROUTINE

  END

