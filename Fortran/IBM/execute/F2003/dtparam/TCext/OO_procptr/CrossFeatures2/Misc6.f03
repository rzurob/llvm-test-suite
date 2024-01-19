! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures2/Misc6.f
! opt variations: -qnol

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
!*  Procedure pointer - access spec
!*  (315097->315733)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: Id
    PROCEDURE(ModFun), PASS, POINTER, PRIVATE :: ProcPtr1=>NULL()
    PROCEDURE(ModFun), PASS, POINTER, PUBLIC  :: ProcPtr2
  END TYPE

  PROCEDURE(ModFun), POINTER, PRIVATE   :: ProcPtr1
  PROCEDURE(ProcPtr1), POINTER, PUBLIC  :: ProcPtr2
  TYPE(DT(20,4)), SAVE                        :: V

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT(*,4)) :: Arg
  TYPE(DT(20,4))  :: ModFun
    ModFun = Arg
  END FUNCTION

  SUBROUTINE Check()

    ProcPtr1 => ModFun
    V = ProcPtr1(DT(20,4)(-1, ModFun, ModFun))
    IF ( V%ID .NE. -1) ERROR STOP 11
    IF ( .NOT. ASSOCIATED(V%ProcPtr1, ModFun) ) ERROR STOP 12
    IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) ERROR STOP 13

    V = DT(20,4)(0, NULL(), NULL())
    ProcPtr2 => ModFun
    V = ProcPtr2(DT(20,4)(-2, ModFun, ModFun))
    IF ( V%ID .NE. -2) ERROR STOP 21
    IF ( .NOT. ASSOCIATED(V%ProcPtr1, ModFun) ) ERROR STOP 22
    IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) ERROR STOP 23

    V = DT(20,4)(0, NULL(), NULL())
    V%ID = -3
    V%ProcPtr1 => ModFun
    V%ProcPtr2 => ModFun
    V = V%ProcPtr1()
    IF ( V%ID .NE. -3) ERROR STOP 31
    IF ( .NOT. ASSOCIATED(V%ProcPtr1, ModFun) ) ERROR STOP 32
    IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) ERROR STOP 33

    V = DT(20,4)(-4, NULL(), NULL())
    V%ProcPtr1 => ModFun
    V%ProcPtr2 => ModFun
    V = V%ProcPtr2()
    IF ( V%ID .NE. -4) ERROR STOP 41
    IF ( .NOT. ASSOCIATED(V%ProcPtr1, ModFun) ) ERROR STOP 42
    IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) ERROR STOP 43

  END SUBROUTINE

  END MODULE

  PROGRAM Misc6
  USE M
  IMPLICIT NONE

  CALL Check()

  V = DT(20,4)(0, ProcPtr2=NULL())
  ProcPtr2 => ModFun
  V = ProcPtr2(DT(20,4)(-5, ProcPtr2=ModFun))
  IF ( V%ID .NE. -5) ERROR STOP 51
  IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) ERROR STOP 53

  V = DT(20,4)(-6, ProcPtr2=NULL())
  V%ProcPtr2 => ModFun
  V = V%ProcPtr2()
  IF ( V%ID .NE. -6) ERROR STOP 61
  IF ( .NOT. ASSOCIATED(V%ProcPtr2, ModFun) ) ERROR STOP 63

  END

