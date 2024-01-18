! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 12, 2005
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
!*  Procedure component reference
!*  proc-component-ref is variable % procedure-component-name
!*
!*  ()
!*
! *********************************************************************

  MODULE M0

    INTERFACE
      FUNCTION IntF(Arg)
        CHARACTER(1) :: IntF, Arg
      END FUNCTION
    END INTERFACE

    TYPE :: DT0
      PROCEDURE (IntF), NOPASS, POINTER :: ProcPtr0 => NULL()
    END TYPE
  CONTAINS

    FUNCTION ModFun(Arg)
    CHARACTER(1) :: ModFun, Arg
      ModFun = Arg
    END FUNCTION

  END MODULE

  MODULE M
  USE M0
    TYPE, EXTENDS(DT0) :: DT
      PROCEDURE (IntF), NOPASS, POINTER :: ProcPtr1 => NULL()
    END TYPE

    TYPE(DT), SAVE :: MV

  END MODULE

  PROGRAM  PtrAssignC725
  USE M
  IMPLICIT NONE

  TYPE, EXTENDS(DT) :: PDT
    PROCEDURE (IntF), NoPASS,  POINTER :: ProcPtr
  END TYPE

  TYPE(PDT)       :: PV
  PROCEDURE(IntF) :: ExtFun

  MV%ProcPtr1 => ModFun
  IF ( MV%ProcPtr1('0') .NE. '0' )            ERROR STOP 11

  PV%DT%ProcPtr0 => ModFun
  IF ( PV%DT%ProcPtr0('a') .NE. 'a' )         ERROR STOP 12

  PV%DT%DT0%ProcPtr0 => ModFun
  IF ( PV%DT%DT0%ProcPtr0('9') .NE. '9' )     ERROR STOP 13

  PV%ProcPtr => ModFun
  IF ( PV%ProcPtr('9') .NE. '9' )            ERROR STOP 14

  PV%ProcPtr => ExtFun
  IF ( PV%ProcPtr('0') .NE. '0' )            ERROR STOP 21

  PV%DT%ProcPtr1 => ExtFun
  IF ( PV%DT%ProcPtr1('a') .NE. 'a' )         ERROR STOP 22

  PV%DT%DT0%ProcPtr0 => ExtFun
  IF ( PV%DT%DT0%ProcPtr0('9') .NE. '9' )     ERROR STOP 23

  PV%DT0%ProcPtr0 => ExtFun
  IF ( PV%DT0%ProcPtr0('9') .NE. '9' )        ERROR STOP 24

  END

  FUNCTION ExtFun(Arg)
  CHARACTER(1) :: ExtFun, Arg
    ExtFun = Arg
  END FUNCTION

