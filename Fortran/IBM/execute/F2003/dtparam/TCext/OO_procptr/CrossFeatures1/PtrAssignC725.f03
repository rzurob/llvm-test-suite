! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/OO_procptr/CrossFeatures1/PtrAssignC725.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

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
!*  (304382)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M0
    TYPE :: DT0(K1)    ! (4)
      INTEGER, KIND :: K1
      PROCEDURE (ModFun), NOPASS, POINTER :: ProcPtr => NULL()
    END TYPE
  CONTAINS
    FUNCTION ModFun(Arg)
    CHARACTER(1) :: ModFun, Arg
      ModFun = Arg
    END FUNCTION
  END MODULE

  MODULE M
  USE M0
    TYPE :: DT(K2)    ! (4)
      INTEGER, KIND :: K2
      TYPE(DT0(K2)) :: Base
      PROCEDURE (ModFun), NOPASS, POINTER :: ProcPtr => NULL()
    END TYPE

    TYPE(DT(4)), SAVE :: MV

  END MODULE


  PROGRAM  PtrAssignC725
  USE M
  IMPLICIT NONE

  TYPE :: PDT(K3)    ! (4)
    INTEGER, KIND :: K3
    TYPE(DT(K3))  :: Child
    PROCEDURE (ModFun), NoPASS,  POINTER :: ProcPtr
  END TYPE

  TYPE(PDT(4)) :: PV
  PROCEDURE(ModFun) :: ExtFun

    PV%ProcPtr => ModFun
    IF ( PV%ProcPtr('0') .NE. '0' )            ERROR STOP 11

    PV%Child%ProcPtr => ModFun
    IF ( PV%Child%ProcPtr('a') .NE. 'a' )      ERROR STOP 12

    PV%Child%Base%ProcPtr => ModFun
    IF ( PV%Child%Base%ProcPtr('9') .NE. '9' ) ERROR STOP 13

    PV%ProcPtr => ModFun
    IF ( PV%ProcPtr('9') .NE. '9' )            ERROR STOP 14

    PV%ProcPtr => ExtFun
    IF ( PV%ProcPtr('0') .NE. '0' )             ERROR STOP 21

    PV%Child%ProcPtr => ExtFun
    IF ( PV%Child%ProcPtr('a') .NE. 'a' )       ERROR STOP 22

    PV%Child%Base%ProcPtr => ExtFun
    IF ( PV%Child%Base%ProcPtr('9') .NE. '9' )  ERROR STOP 23

    PV%Child%ProcPtr => ExtFun
    IF ( PV%Child%ProcPtr('9') .NE. '9' )       ERROR STOP 24


  END


  FUNCTION ExtFun(Arg)
  CHARACTER(1) :: ExtFun, Arg
    ExtFun = Arg
  END FUNCTION
