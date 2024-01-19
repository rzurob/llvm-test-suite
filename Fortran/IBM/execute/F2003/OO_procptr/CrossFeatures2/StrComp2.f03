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
!*  Structure component - parent component
!*  (315447)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1=>NULL()
    END TYPE

    TYPE, EXTENDS(Base)  :: DT
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2=>NULL()
      TYPE(Base) :: BComp
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

  END MODULE

  PROGRAM StrComp2
  USE M
  IMPLICIT NONE

  TYPE(DT) :: V=DT(Base=Base(NULL()), ProcPtr2=NULL(), BComp=Base(NULL()))

  V = DT(Base=Base(ModFun1), ProcPtr2=ModFun2, BComp=Base(ModFun1))

  IF ( .NOT. ASSOCIATED(V%ProcPtr1))       ERROR STOP 11
  IF ( .NOT. ASSOCIATED(V%BComp%ProcPtr1)) ERROR STOP 12
  IF ( .NOT. ASSOCIATED(V%ProcPtr2))       ERROR STOP 13

  IF ( V%Base%ProcPtr1(4.0)  .NE. 4.0 )   ERROR STOP 21
  IF ( V%BComp%ProcPtr1(4.0) .NE. 4.0 )   ERROR STOP 22
  IF ( V%ProcPtr2(8.0_8)      .NE. 8.0_8 ) ERROR STOP 23

  END

