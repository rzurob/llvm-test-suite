! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Final2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Final2.f
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
!*  Finalization - executable construct
!*  (ice)
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

  END MODULE


  PROGRAM Final2
  USE M
  TYPE(DT), ALLOCATABLE  :: V
  CLASS(DT), POINTER     :: U

  IF ( ANY (Trace .NE. (/0,0,0,0/))) STOP 11
  ALLOCATE(V, SOURCE=DT(ModFun1, ModFun2))
  IF ( ANY (Trace .NE. (/2,1,0,0/))) STOP 12

  Trace = 0
  Index = 0
  ASSOCIATE ( As => DT(ModFun1, ModFun2) )
  END ASSOCIATE
  IF ( ANY (Trace .NE. (/2,1,0,0/))) STOP 13

  Trace = 0
  Index = 0
  ALLOCATE(U, SOURCE=DT(ModFun1, ModFun2))
  IF ( ANY (Trace .NE. (/2,1,0,0/))) STOP 14

  Trace = 0
  Index = 0
  SELECT TYPE ( As => V%ProcPtr1() )
  CLASS IS (Base)
    IF ( ANY (Trace .NE. (/0,0,0,0/))) STOP 15
  CLASS DEFAULT
    STOP 21
  END SELECT
  IF ( ANY (Trace .NE. (/0,0,0,0/)))   STOP 16

  Trace = 0
  Index = 0
  SELECT TYPE ( As =>  V%ProcPtr2() )
  CLASS IS (DT)
    IF ( ANY (Trace .NE. (/0,0,0,0/))) STOP 17
  CLASS DEFAULT
    STOP 23
  END SELECT
  IF ( ANY (Trace .NE. (/2,1,0,0/)))   STOP 18

  END

