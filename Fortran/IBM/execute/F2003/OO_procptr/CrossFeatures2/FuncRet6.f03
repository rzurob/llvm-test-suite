! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 28, 2005
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
!*  Function Return - entry stmt
!*  (315015/316895)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT
      PROCEDURE(ModFun), PASS, POINTER :: ProcPtr
      CONTAINS
      PROCEDURE, PASS :: Proc => ModFun
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      TYPE(DT), ALLOCATABLE :: T1
      TYPE(DT), POINTER     :: T2
    END TYPE

  CONTAINS

    FUNCTION ModFun(Arg) RESULT(ResModFun)
    CLASS(DT) :: Arg
    PROCEDURE(ModFun), POINTER :: ResModFun, ResModFun1, ResModFun2
      ResModFun => Arg%ProcPtr
      RETURN

    ENTRY ModFun1(Arg) RESULT(ResModFun1)
      ResModFun1 => Arg%ProcPtr
      RETURN

    ENTRY ModFun2(Arg) RESULT(ResModFun2)
      ResModFun2 => Arg%ProcPtr

    END FUNCTION

  END MODULE

  PROGRAM FuncRet6
  USE M
  IMPLICIT TYPE(DT1)(P)

  PROCEDURE(ModFun1),   POINTER :: ProcPtr1
  PROCEDURE(ModFun2),   POINTER :: ProcPtr2
  TYPE(DT)                      ::  Const=DT(  NULL())


  TYPE(DT1)        :: V(10000)
  TYPE(DT), TARGET :: Tar

  Const%ProcPtr => modFun
  Tar = Const
  V = DT1( ModFun, Const, Tar)

  CALL IntSub( ModFun(V(1000)), ModFun)

  DO I=1, 10000
    CALL IntSub( V(I)%T1%ProcPtr, ModFun )
    CALL IntSub( V(I)%T2%ProcPtr, ModFun )

    CALL IntSub( V(I)%T1%Proc(), ModFun )
    CALL IntSub( V(I)%T2%Proc(), ModFun )
  END DO

  V = DT1( ModFun1, NULL(), NULL())
  ProcPtr1 => ModFun(V(10000))
  CALL IntSub( ProcPtr1, ModFun1 )

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  PROCEDURE(ModFun1), POINTER :: Arg1
  PROCEDURE(ModFun2)          :: Arg2
      IF ( .NOT. ASSOCIATED(Arg1, Arg2))  ERROR STOP 66
  END SUBROUTINE

  END

