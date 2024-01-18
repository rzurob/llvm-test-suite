! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_procptr/CrossFeatures2/Final4.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
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

    TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1=>NULL()
    CONTAINS
      FINAL :: FinalBase
    END TYPE

    TYPE, EXTENDS(Base)  :: DT    ! (4,20)
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2=>NULL()
    CONTAINS
      PROCEDURE, PASS :: ProcPtr => ModFun3
      FINAL :: FinalDT
    END TYPE

    INTEGER :: Trace(4)=0
    INTEGER :: Index=0

    CONTAINS

    SUBROUTINE FinalDT(Arg)
    TYPE(DT(4,*)) :: Arg
      Index = Index + 1
      TracE(Index) = 2
      Print*, "Final DT"
    END SUBROUTINE

    SUBROUTINE FinalBase(Arg)
    TYPE(Base(4,*)) :: Arg
      Index = Index + 1
      TracE(Index) = 1
      Print*, "Final Base"
    END SUBROUTINE

    FUNCTION ModFun1(Arg)
    CLASS(Base(4,*)) :: Arg
    CLASS(Base(4,:)), POINTER :: ModFun1
      ALLOCATE(ModFun1, SOURCE=Arg)
    END FUNCTION

    FUNCTION ModFun2(Arg)
    CLASS(DT(4,*)) :: Arg
    CLASS(DT(4,:)), ALLOCATABLE :: ModFun2
      ALLOCATE(ModFun2, SOURCE=Arg)
    END FUNCTION

    FUNCTION ModFun3(Arg1, Arg2)
    CLASS(DT(4,*)) :: Arg1
    PROCEDURE(ModFun1), POINTER :: ModFun3, Arg2
      ModFun3 => Arg2
    END FUNCTION

    SUBROUTINE ModSub(Arg1, Arg2)
    TYPE(DT(4,*)), INTENT(OUT) :: Arg1, Arg2
    END SUBROUTINE

  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(DT(4,*)), INTENT(OUT) :: Arg1, Arg2
  END SUBROUTINE


  PROGRAM Final4
  USE M
  TYPE(DT(4,20)) :: V

  PROCEDURE(ModSub)          :: ExtSub
  PROCEDURE(ExtSub), POINTER :: ProcPtr

  IF ( ANY (Trace .NE. (/0,0,0,0/))) ERROR STOP 11
  CALL IntSub(V, V)
  IF ( ANY (Trace .NE. (/2,1,2,1/))) ERROR STOP 12

  Trace=0
  Index = 0
  ProcPtr => ModSub
  CALL ProcPtr(V, V)
  IF ( ANY (Trace .NE. (/2,1,2,1/))) ERROR STOP 12

  Trace=0
  Index = 0
  ProcPtr => ExtSub
  CALL ProcPtr(V, V)
  IF ( ANY (Trace .NE. (/2,1,2,1/))) ERROR STOP 12

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  TYPE(DT(4,*)), INTENT(OUT) :: Arg1, Arg2
  END SUBROUTINE

  END

