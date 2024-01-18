! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures2/Misc2.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Misc2.f 
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
!*  TEST CASE NAME             : Misc2.f 
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
!*  Procedure pointer - argument/enum 
!*  (core-struct contr problem )
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  ENUM, BIND(C)
    ENUMERATOR :: Zero 
    ENUMERATOR :: One
    ENUMERATOR :: Two
    ENUMERATOR :: Thr
  END ENUM

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: Id = One
    PROCEDURE(IFun), NoPASS, POINTER :: ProcPtr
    CONTAINS
    PROCEDURE, NoPASS :: Proc => ModFun 
  END TYPE

  ENUM, BIND(C)
    ENUMERATOR :: Start 
    ENUMERATOR :: Mon
    ENUMERATOR :: Tue
    ENUMERATOR :: Wed
    ENUMERATOR :: Thu
    ENUMERATOR :: Fri
    ENUMERATOR :: Sat
    ENUMERATOR :: Sun
  END ENUM



  CONTAINS


  FUNCTION IFun(Arg)
  TYPE(DT(*,4)) :: Arg
  TYPE(DT(20,4)) :: IFun
    IFun = Arg
  END FUNCTION

  FUNCTION ModFun(Arg)
  TYPE(DT(*,4)) :: Arg
  TYPE(DT(20,4)) :: ModFun
    ModFun = Arg
  END FUNCTION

  END MODULE

  PROGRAM Misc2 
  USE M

  PROCEDURE(ModFun),        POINTER :: ProcPtr1
  PROCEDURE(ProcPtr1),      POINTER :: ProcPtr2
  PROCEDURE(ModFun),        POINTER :: ProcPtr3
  TYPE(DT(20,4))                    :: Const=DT(20,4)(Mon, NULL())
  TYPE(DT(20,4))                    :: U 


  TYPE(DT(20,4))        :: V(Thr) 

  Const%ProcPtr => modFun
  V = Const
 
  ProcPtr1 => ModFun
  ProcPtr2 => ProcPtr1
 
  U = ProcPtr1(DT(20,4)(Two, ModFun) )
  IF ( U%ID .NE.  2 )                       STOP 11
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun)) STOP 12

  U = ProcPtr2(DT(20,4)(Thr, ModFun) )
  IF ( U%ID .NE.  3 )                       STOP 21
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun)) STOP 22

  U = V(One)%ProcPtr(V(Two))
  IF ( U%ID .NE.  1 )                       STOP 31
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun)) STOP 32

  U = DT(20,4)(Sun, NULL())
  U = V(Thr)%Proc(V(One))
  IF ( U%ID .NE.  1 )                       STOP 41
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun)) STOP 42

  ProcPtr3 => ModFun
  U = ProcPtr3(DT(20,4)(Two, ModFun) )
  IF ( U%ID .NE.  2 )                       STOP 51
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun)) STOP 52

  END

