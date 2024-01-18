! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv /tstdev/OO_procptr/CrossFeatures1/PtrAssignC7251.f
! opt variations: -qnock -qnok -qnol -qdefaultpv

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp PtrAssignC7251.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignC7251.f
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
!* C725 (R741) the procedure-component-name shall be the name of a
!* procedure pointer component of the declared type of variable.
!*
!*  (304382)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1,N1)    ! (1,1)
      INTEGER, KIND                        :: K1
      INTEGER, LEN                         :: N1
      CHARACTER(kind=K1,len=N1), POINTER   :: CharPtr => NULL()
      PROCEDURE (ModFun), POINTER, NOPASS  :: ProcPtr => NULL()
    END TYPE

  TYPE(DT(1,1)), SAVE :: MV

  CONTAINS
    FUNCTION ModFun(Arg)
    CHARACTER(1) :: ModFun, Arg
      ModFun = Arg
    END FUNCTION
  END MODULE


  PROGRAM  PtrAssignC7251
  USE M
  IMPLICIT NONE

  TYPE :: PDT(K2,N2,K3,N3)    ! (4,20,1,1)
    INTEGER, KIND   :: K2,K3
    INTEGER, LEN    :: N2,N3
    TYPE(DT(K3,N3)) :: Base
    PROCEDURE (CHARACTER(1)), NOPASS, POINTER :: ProcPtr
  END TYPE

  TYPE(PDT(4,20,1,1)) :: LV

  PROCEDURE(CHARACTER(1)) :: ExtFun


    MV%CharPtr => ModFun
    MV%CharPtr => ExtFun
    MV%ProcPtr => ExtFun

    LV%Base%CharPtr => ModFun
    LV%Base%CharPtr => ExtFun
    LV%Base%ProcPtr => MV%CharPtr

    LV%ProcPtr => ModFun
    LV%ProcPtr => MV%CharPtr
    LV%ProcPtr => MV%ProcPtr

    LV%Base%CharPtr => ModFun

  END


  FUNCTION ExtFun(Arg)
  CHARACTER(1) :: ExtFun, Arg
    ExtFun = Arg
  END FUNCTION

