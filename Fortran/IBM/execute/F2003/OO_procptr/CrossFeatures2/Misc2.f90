! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 26, 2005
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

  TYPE :: DT
    INTEGER :: Id = One
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
  TYPE(DT) :: IFun, Arg
    IFun = Arg
  END FUNCTION

  FUNCTION ModFun(Arg)
  TYPE(DT) :: Arg
  TYPE(DT) :: ModFun
    ModFun = Arg
  END FUNCTION

  END MODULE

  PROGRAM Misc2
  USE M

  PROCEDURE(ModFun),        POINTER :: ProcPtr1
  PROCEDURE(ProcPtr1),      POINTER :: ProcPtr2
  PROCEDURE(TYPE(DT)),      POINTER :: ProcPtr3
  TYPE(DT)                          :: Const=DT(Mon, NULL())
  TYPE(DT)                          :: U


  TYPE(DT)        :: V(Thr)

  Const%ProcPtr => modFun
  V = Const

  ProcPtr1 => ModFun
  ProcPtr2 => ProcPtr1

  U = ProcPtr1(DT(Two, ModFun) )
  IF ( U%ID .NE.  2 )                       ERROR STOP 11
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun)) ERROR STOP 12

  U = ProcPtr2(DT(Thr, ModFun) )
  IF ( U%ID .NE.  3 )                       ERROR STOP 21
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun)) ERROR STOP 22

  U = V(One)%ProcPtr(V(Two))
  IF ( U%ID .NE.  1 )                       ERROR STOP 31
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun)) ERROR STOP 32

  U = DT(Sun, NULL())
  U = V(Thr)%Proc(V(One))
  IF ( U%ID .NE.  1 )                       ERROR STOP 41
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun)) ERROR STOP 42

  ProcPtr3 => ModFun
  U = ProcPtr3(DT(Two, ModFun) )
  IF ( U%ID .NE.  2 )                       ERROR STOP 51
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun)) ERROR STOP 52

  END

