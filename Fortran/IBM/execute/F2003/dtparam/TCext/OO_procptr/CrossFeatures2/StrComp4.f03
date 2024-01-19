! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=base /tstdev/OO_procptr/CrossFeatures2/StrComp4.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 24, 2005
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
!*  Structure component - parameter
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1=>NULL()
    END TYPE

    TYPE, EXTENDS(Base)  :: DT    ! (4,20)
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2=>NULL()
      TYPE(Base(K1,N1)) :: BComp
    CONTAINS
      PROCEDURE, PASS :: TypeProc => ModFun3
    END TYPE

    CONTAINS

    FUNCTION ModFun1(Arg1, Arg2)
    CLASS(Base(4,*)) :: Arg1
    REAL(4) :: ModFun1, Arg2
      ModFun1 = Arg2
    END FUNCTION

    FUNCTION ModFun2(Arg1, Arg2)
    CLASS(DT(4,*)) :: Arg1
    REAL(8) :: ModFun2, Arg2
      ModFun2 = Arg2
    END FUNCTION

    FUNCTION ModFun3(Arg1, Arg2)
    CLASS(DT(4,*)) :: Arg1
    PROCEDURE(ModFun1), POINTER :: ModFun3, Arg2
      ModFun3 => Arg2
    END FUNCTION

  END MODULE

  PROGRAM StrComp4
  USE M
  IMPLICIT NONE

  TYPE(DT(4,20)),PARAMETER  :: V=DT(4,20)(Base=Base(4,20)(NULL()), ProcPtr2=NULL(), BComp=Base(4,20)(NULL()))
  PROCEDURE(ModFun1), POINTER :: ProcPtr
  TYPE(DT(4,20))  :: U

  IF ( ASSOCIATED(V%ProcPtr1))       ERROR STOP 11
  IF ( ASSOCIATED(V%BComp%ProcPtr1)) ERROR STOP 12
  IF ( ASSOCIATED(V%ProcPtr2))       ERROR STOP 13

  ProcPtr => ModFun1
  U = DT(4,20)(Base=V%Base, ProcPtr2=V%ProcPtr2, BComp=V%BComp)
  IF ( ASSOCIATED(U%ProcPtr1))       ERROR STOP 21
  IF ( ASSOCIATED(U%BComp%ProcPtr1)) ERROR STOP 22
  IF ( ASSOCIATED(U%ProcPtr2))       ERROR STOP 23

  ProcPtr => V%TypeProc(V%ProcPtr1)
  IF ( ASSOCIATED(ProcPtr) )         ERROR STOP 21

  END

