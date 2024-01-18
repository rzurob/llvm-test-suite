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

    TYPE :: Base
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1=>NULL()
    END TYPE

    TYPE, EXTENDS(Base)  :: DT
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2=>NULL()
      TYPE(Base) :: BComp
    CONTAINS
      PROCEDURE, PASS :: TypeProc => ModFun3
    END TYPE

    CONTAINS

    FUNCTION ModFun1(Arg1, Arg2)
    CLASS(Base) :: Arg1
    REAL(4) :: ModFun1, Arg2
      ModFun1 = Arg2
    END FUNCTION

    FUNCTION ModFun2(Arg1, Arg2)
    CLASS(DT) :: Arg1
    REAL(8) :: ModFun2, Arg2
      ModFun2 = Arg2
    END FUNCTION

    FUNCTION ModFun3(Arg1, Arg2)
    CLASS(DT) :: Arg1
    PROCEDURE(ModFun1), POINTER :: ModFun3, Arg2
      ModFun3 => Arg2
    END FUNCTION

  END MODULE

  PROGRAM StrComp4
  USE M
  IMPLICIT NONE

  TYPE(DT),PARAMETER  :: V=DT(Base=Base(NULL()), ProcPtr2=NULL(), BComp=Base(NULL()))
  PROCEDURE(ModFun1), POINTER :: ProcPtr
  TYPE(DT)  :: U

  IF ( ASSOCIATED(V%ProcPtr1))       STOP 11
  IF ( ASSOCIATED(V%BComp%ProcPtr1)) STOP 12
  IF ( ASSOCIATED(V%ProcPtr2))       STOP 13

  ProcPtr => ModFun1
  U = DT(Base=V%Base, ProcPtr2=V%ProcPtr2, BComp=V%BComp)
  IF ( ASSOCIATED(U%ProcPtr1))       STOP 21
  IF ( ASSOCIATED(U%BComp%ProcPtr1)) STOP 22
  IF ( ASSOCIATED(U%ProcPtr2))       STOP 23

  ProcPtr => V%TypeProc(V%ProcPtr1)
  IF ( ASSOCIATED(ProcPtr) )         STOP 21

  END

