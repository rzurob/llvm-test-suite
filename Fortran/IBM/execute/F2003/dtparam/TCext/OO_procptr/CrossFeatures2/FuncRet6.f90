! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_procptr/CrossFeatures2/FuncRet6.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: FuncRet6.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncRet6.f
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

    TYPE :: DT(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      PROCEDURE(ModFun), PASS, POINTER :: ProcPtr
      CONTAINS
      PROCEDURE, PASS :: Proc => ModFun
    END TYPE

    TYPE, EXTENDS(DT) :: DT1    ! (4,20)
      TYPE(DT(K1,:)), ALLOCATABLE :: T1
      TYPE(DT(K1,:)), POINTER     :: T2
    END TYPE

  CONTAINS

    FUNCTION ModFun(Arg) RESULT(ResModFun)
    CLASS(DT(4,*)) :: Arg
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
  IMPLICIT TYPE(DT1(4,20))(P)

  PROCEDURE(ModFun1),   POINTER :: ProcPtr1
  PROCEDURE(ModFun2),   POINTER :: ProcPtr2
  TYPE(DT(4,20))                      ::  Const=DT(4,20)(  NULL())


  TYPE(DT1(4,20))        :: V(10000)
  TYPE(DT(4,20)), TARGET :: Tar

  Const%ProcPtr => modFun
  Tar = Const
  V = DT1(4,20)( ModFun, Const, Tar)

  CALL IntSub( ModFun(V(1000)), ModFun)

  DO I=1, 10000
    CALL IntSub( V(I)%T1%ProcPtr, ModFun )
    CALL IntSub( V(I)%T2%ProcPtr, ModFun )

    CALL IntSub( V(I)%T1%Proc(), ModFun )
    CALL IntSub( V(I)%T2%Proc(), ModFun )
  END DO

  V = DT1(4,20)( ModFun1, NULL(), NULL())
  ProcPtr1 => ModFun(V(10000))
  CALL IntSub( ProcPtr1, ModFun1 )

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  PROCEDURE(ModFun1), POINTER :: Arg1
  PROCEDURE(ModFun2)          :: Arg2
      IF ( .NOT. ASSOCIATED(Arg1, Arg2))  STOP 66
  END SUBROUTINE

  END

