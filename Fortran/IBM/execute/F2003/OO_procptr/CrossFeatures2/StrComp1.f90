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
!*  Structure component - sequence type
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
      SEQUENCE
      PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE  :: DT
      SEQUENCE
      PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
      TYPE(Base) :: BComp
    END TYPE


    CONTAINS

    FUNCTION ModFun1(Arg)
    REAL(8) :: ModFun1, Arg
      ModFun1 = Arg
    END FUNCTION

    FUNCTION ModFun2(Arg)
    REAL(8) :: ModFun2, Arg
      ModFun2 = Arg
    END FUNCTION

  END MODULE


  SUBROUTINE ExtSub(Arg1, Arg2 )

  TYPE :: Base
    SEQUENCE
    PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
  END TYPE

  TYPE  :: DT
    SEQUENCE
    PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
    TYPE(Base) :: BComp
  END TYPE

  REAL(8)  :: Arg2
  TYPE(DT) :: Arg1

    IF ( .NOT. ASSOCIATED(Arg1%ProcPtr))       ERROR STOP 11
    IF ( .NOT. ASSOCIATED(Arg1%BComp%ProcPtr)) ERROR STOP 12
    IF ( Arg1%ProcPtr(Arg2)       .NE. Arg2 )  ERROR STOP 13
    IF ( Arg1%BComp%ProcPtr(Arg2) .NE. Arg2 )  ERROR STOP 14

  END SUBROUTINE


  PROGRAM StrComp1
  USE M
  IMPLICIT NONE

  INTERFACE
    SUBROUTINE ExtSub(Arg1, Arg2 )
      IMPORT DT
      REAL(8)  :: Arg2
      TYPE(DT) :: Arg1
    END SUBROUTINE
  END INTERFACE

  TYPE(DT) :: V

  V%ProcPtr => ModFun2
  V%BComp%ProcPtr => ModFun1

  CALL ExtSub(V, 8.0_8)
  CALL ExtSub(V, -8.0_8)

  END

