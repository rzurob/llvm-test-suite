! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_procptr/CrossFeatures2/Final3.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

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
!*  Finalization - Structure constructor
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1
    CONTAINS
      FINAL :: FinalBase
    END TYPE

    TYPE, EXTENDS(Base)  :: DT(K2,N2)    ! (4,20,4,20)
      INTEGER, KIND     :: K2
      INTEGER, LEN      :: N2
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2
      TYPE(Base(K2,N2)) :: BComp
    CONTAINS
      PROCEDURE, PASS :: ProcPtr => ModFun3
      FINAL :: FinalDT
    END TYPE

    INTEGER :: Trace(8)=0
    INTEGER :: Index=0

    CONTAINS

    SUBROUTINE FinalDT(Arg)
    TYPE(DT(4,*,4,*)) :: Arg
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
    CLASS(DT(4,*,4,*)) :: Arg
    CLASS(DT(4,:,4,:)), ALLOCATABLE :: ModFun2
      ALLOCATE(ModFun2, SOURCE=Arg)
    END FUNCTION

    FUNCTION ModFun3(Arg1, Arg2)
    CLASS(DT(4,*,4,*)) :: Arg1
    PROCEDURE(ModFun1), POINTER :: ModFun3, Arg2
      ModFun3 => Arg2
    END FUNCTION

  END MODULE


  PROGRAM Final3
  USE M
  TYPE(DT(4,20,4,20)) :: V
  PROCEDURE(ModFun2), POINTER :: ProcPtr

  type(Base(4,20)) :: baseModFun1

  baseModFun1%ProcPtr1 => Modfun1

  IF ( ANY (Trace .NE. 0)) STOP 11
  V = DT(4,20,4,20)(Base=BaseModfun1, BComp=BaseModfun1, ProcPtr2=ModFun2)
  IF ( ANY (Trace .NE. (/2,1,1,2,1,1,0,0/))) STOP 12

  Trace=0
  Index = 0
  V%Base = V%ProcPtr1()
  IF ( ANY (Trace .NE. (/1,0,0,0,0,0,0,0/))) STOP 13

  Trace=0
  Index = 0
  V = V%ProcPtr2()
  IF ( ANY (Trace .NE. (/2,1,1,2,1,1,0,0/))) STOP 14

  ProcPtr => ModFun2
  Trace=0
  Index = 0
  V = procPtr(V)
  IF ( ANY (Trace .NE. (/2,1,1,2,1,1,0,0/))) STOP 15

  END

