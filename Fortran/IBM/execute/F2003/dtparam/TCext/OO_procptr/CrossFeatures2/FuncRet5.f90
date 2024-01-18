! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_procptr/CrossFeatures2/FuncRet5.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: FuncRet5.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncRet5.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 26, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 289058 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*   
!*  Function Return - Procedure pointer 
!*  (ICE-314926/(315012/316119)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      PROCEDURE(IFun), PASS, POINTER :: ProcPtr
      CONTAINS
      PROCEDURE, PASS :: Proc => ModFun 
    END TYPE

    INTERFACE 
      FUNCTION IFun(Arg)
        IMPORT DT
        CLASS(DT(4)) :: Arg
        TYPE(DT(4))  :: IFun
      END FUNCTION
    END INTERFACE

    TYPE, EXTENDS(DT) :: DT1    ! (4)
      TYPE(DT(K1)), ALLOCATABLE :: T1
      TYPE(DT(K1)), POINTER     :: T2
    END TYPE

  CONTAINS

   FUNCTION ModFun1(Arg)
   TYPE(DT(4)) :: ModFun1
   CLASS(DT(4)) :: Arg
     ModFun1=Arg
   END FUNCTION

    FUNCTION ModFun(Arg)
    CLASS(DT(4)) :: Arg
    PROCEDURE(IFun), POINTER :: ModFun
      ModFun => Arg%ProcPtr 
    END FUNCTION

  END MODULE

  PROGRAM FuncRet5
  USE M
  IMPLICIT TYPE(DT1(4))(P) 

  PROCEDURE(ModFun),        POINTER :: ProcPtr1
  PROCEDURE(ProcPtr1),      POINTER :: ProcPtr2
  TYPE(DT(4))                       ::  Const=DT(4)(  NULL()), T


  TYPE(DT1(4))        :: V(10000) 
  TYPE(DT(4)), TARGET :: Tar

  Const%ProcPtr => ModFun1
  Tar = Const 
  V = DT1(4)(ModFun1, Const, Tar)

  IF (.NOT. ASSOCIATED(V(1)%PROCPTR, ModFun1)) STOP 33
  T = V(1)%ProcPtr()
  IF (.NOT. ASSOCIATED(T%PROCPTR, ModFun1)) STOP 34

  CALL IntSub( ModFun(V(1)), ModFun1)

  ProcPtr1 => ModFun(V(1000))
  CALL IntSub( ProcPtr1, ModFun1)

  DO I=1, 10000
    CALL IntSub( V(I)%T1%ProcPtr, ModFun1 )
    CALL IntSub( V(I)%T2%ProcPtr, ModFun1 )

    CALL IntSub( V(I)%T1%Proc(), ModFun1 )
    CALL IntSub( V(I)%T2%Proc(), ModFun1 )
  END DO

  CONTAINS
  
  SUBROUTINE IntSub(Arg1, Arg2)
  PROCEDURE(ModFun1), POINTER :: Arg1
  PROCEDURE(ModFun1)          :: Arg2
      IF ( .NOT. ASSOCIATED(Arg1, Arg2)) STOP 66
  END SUBROUTINE

  END

