! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 11, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  Procedure pointer component
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M0
  CONTAINS
    FUNCTION ModFun(Arg)
    INTEGER(1) :: ModFun, Arg
      ModFun = Arg
    END FUNCTION
  END MODULE

  MODULE M
  USE M0
    TYPE :: MDT
      PROCEDURE (ModFun), POINTER, NOPASS :: ProcPtr => NULL()
    END TYPE

    TYPE(MDT), SAVE :: MV

  END MODULE


  PROGRAM  PtrAssignComp
  USE M, LV => MV, LModFun => ModFun
  IMPLICIT NONE

  TYPE :: PDT
    PROCEDURE (LModFun), POINTER, NOPASS :: ProcPtr
  END TYPE

  TYPE(PDT) :: PV

    LV%ProcPtr => LModFun
    IF ( LV%ProcPtr(10_1) .NE. 10_1 ) ERROR STOP 11

    PV%ProcPtr => LModFun
    IF ( PV%ProcPtr(1_1) .NE. 1_1 )  ERROR STOP 12

    IF ( LV%ProcPtr(LV%ProcPtr(1_1)) .NE. 1_1 )  ERROR STOP 13
    IF ( PV%ProcPtr(PV%ProcPtr(2_1)) .NE. 2_1 )  ERROR STOP 14

    IF ( LV%ProcPtr(PV%ProcPtr(127_1))  .NE. 127_1 )   ERROR STOP 15
    IF ( PV%ProcPtr(LV%ProcPtr(-128_1)) .NE. -128_1 )  ERROR STOP 16

  END

