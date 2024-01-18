! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Final.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Final.f
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
!*  Finalization - allocate/deallocate
!*  (ice-314926)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1
    CONTAINS
      FINAL :: FinalBase
    END TYPE

    TYPE, EXTENDS(Base)  :: DT
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2
    CONTAINS
      PROCEDURE, PASS :: ProcPtr => ModFun3
      FINAL :: FinalDT
    END TYPE

    INTEGER :: Trace(5)=0
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

  END MODULE

  PROGRAM Final0
  USE M
  TYPE(DT), POINTER :: V

  ALLOCATE(V, SOURCE=DT(Base=Base(ModFun1), ProcPTr2=Modfun2))

  IF ( .NOT. ASSOCIATED(V%ProcPtr1, Modfun1)) STOP 11
  IF ( .NOT. ASSOCIATED(V%ProcPtr2, Modfun2)) STOP 12

  IF ( ANY (Trace .NE. (/2,1,1,0,0/))) STOP 14

  DEALLOCATE(V)

  IF ( ANY (Trace .NE. (/2,1,1,2,1/))) STOP 15

  END

