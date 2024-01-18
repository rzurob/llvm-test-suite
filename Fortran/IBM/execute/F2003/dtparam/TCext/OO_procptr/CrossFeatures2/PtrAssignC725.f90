! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures2/PtrAssignC725.f
! opt variations: -qnok -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignC725.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignC725.f
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

    TYPE :: DT0(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
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
    TYPE, EXTENDS(DT0) :: DT    ! (4,20)
      PROCEDURE (IntF), NOPASS, POINTER :: ProcPtr1 => NULL()
    END TYPE

    TYPE(DT(4,20)), SAVE :: MV

  END MODULE

  PROGRAM  PtrAssignC725
  USE M
  IMPLICIT NONE

  TYPE, EXTENDS(DT) :: PDT    ! (4,20)
    PROCEDURE (IntF), NoPASS,  POINTER :: ProcPtr
  END TYPE

  TYPE(PDT(4,20))       :: PV
  PROCEDURE(IntF) :: ExtFun

  MV%ProcPtr1 => ModFun
  IF ( MV%ProcPtr1('0') .NE. '0' )            STOP 11

  PV%DT%ProcPtr0 => ModFun
  IF ( PV%DT%ProcPtr0('a') .NE. 'a' )         STOP 12

  PV%DT%DT0%ProcPtr0 => ModFun
  IF ( PV%DT%DT0%ProcPtr0('9') .NE. '9' )     STOP 13

  PV%ProcPtr => ModFun
  IF ( PV%ProcPtr('9') .NE. '9' )            STOP 14

  PV%ProcPtr => ExtFun
  IF ( PV%ProcPtr('0') .NE. '0' )            STOP 21

  PV%DT%ProcPtr1 => ExtFun
  IF ( PV%DT%ProcPtr1('a') .NE. 'a' )         STOP 22

  PV%DT%DT0%ProcPtr0 => ExtFun
  IF ( PV%DT%DT0%ProcPtr0('9') .NE. '9' )     STOP 23

  PV%DT0%ProcPtr0 => ExtFun
  IF ( PV%DT0%ProcPtr0('9') .NE. '9' )        STOP 24

  END

  FUNCTION ExtFun(Arg)
  CHARACTER(1) :: ExtFun, Arg
    ExtFun = Arg
  END FUNCTION

