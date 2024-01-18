! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Final4.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Final4.f
!*
!*  DATE                       : Jun. 25, 2005
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
!*  Finalization - Intent(Out)
!*  (ice)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1=>NULL()
    CONTAINS
      FINAL :: FinalBase
    END TYPE

    TYPE, EXTENDS(Base)  :: DT
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2=>NULL()
    CONTAINS
      PROCEDURE, PASS :: ProcPtr => ModFun3
      FINAL :: FinalDT
    END TYPE

    INTEGER :: Trace(4)=0
    INTEGER :: Index=0

    CONTAINS

    SUBROUTINE FinalDT(Arg)
    TYPE(DT) :: Arg
      Index = Index + 1
      TracE(Index) = 2
      Print*, "Final DT"
    END SUBROUTINE

    SUBROUTINE FinalBase(Arg)
    TYPE(Base) :: Arg
      Index = Index + 1
      TracE(Index) = 1
      Print*, "Final Base"
    END SUBROUTINE

    FUNCTION ModFun1(Arg)
    CLASS(Base) :: Arg
    CLASS(Base), POINTER :: ModFun1
      ALLOCATE(ModFun1, SOURCE=Arg)
    END FUNCTION

    FUNCTION ModFun2(Arg)
    CLASS(DT) :: Arg
    CLASS(DT), ALLOCATABLE :: ModFun2
      ALLOCATE(ModFun2, SOURCE=Arg)
    END FUNCTION

    FUNCTION ModFun3(Arg1, Arg2)
    CLASS(DT) :: Arg1
    PROCEDURE(ModFun1), POINTER :: ModFun3, Arg2
      ModFun3 => Arg2
    END FUNCTION

    SUBROUTINE ModSub(Arg1, Arg2)
    TYPE(DT), INTENT(OUT) :: Arg1, Arg2
    END SUBROUTINE

  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(DT), INTENT(OUT) :: Arg1, Arg2
  END SUBROUTINE


  PROGRAM Final4
  USE M
  TYPE(DT) :: V

  PROCEDURE()          :: ExtSub
  PROCEDURE(), POINTER :: ProcPtr

  IF ( ANY (Trace .NE. (/0,0,0,0/))) STOP 11
  CALL IntSub(V, V)
  IF ( ANY (Trace .NE. (/2,1,2,1/))) STOP 12

  Trace=0
  Index = 0
  ProcPtr => ModSub
  CALL ProcPtr(V, V)
  IF ( ANY (Trace .NE. (/2,1,2,1/))) STOP 12

  Trace=0
  Index = 0
  ProcPtr => ExtSub
  CALL ProcPtr(V, V)
  IF ( ANY (Trace .NE. (/2,1,2,1/))) STOP 12

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  TYPE(DT), INTENT(OUT) :: Arg1, Arg2
  END SUBROUTINE

  END

