! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures2/Allocate1.f
! opt variations: -ql

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
! %POSTCMD: tcomp Allocate1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Allocate1.f
!*
!*  DATE                       : May. 9, 2005
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
!*  The allocate stmt
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id
      PROCEDURE(INTEGER),NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION ModFun()
    TYPE(DT(4)) :: ModFun
      ModFun = DT(4)(-1, NULL())
    END FUNCTION

  END MODULE

  PROGRAM Allocate1
  USE M
  IMPLICIT NONE

  PROCEDURE(TYPE(DT(4))), POINTER :: ProcPtr=>NULL()
  TYPE ( DT(4) ),         POINTER :: V
  PROCEDURE(INTEGER)           :: Fun


  ALLOCATE(ProcPtr)
  ALLOCATE(ProcPtr, SOURCE=Fun)
  ALLOCATE(ProcPtr, SOURCE=DT(4)())
  ALLOCATE(ProcPtr, SOURCE=Fun(0))

  ProcPtr => ModFun
  DEALLOCATE(ProcPtr)

  ALLOCATE(V%ProcPtr)
  ALLOCATE(V%ProcPtr, SOURCE=DT(4)(Fun(0), NULL()))
  ALLOCATE(V%ProcPtr, SOURCE=ProcPtr)
  ALLOCATE(V%ProcPtr, SOURCE=DT(4)())
  ALLOCATE(V%ProcPtr, SOURCE=Fun(0))

  V%ProcPtr => Fun
  DEALLOCATE(V%ProcPtr)

  END

  FUNCTION Fun(Arg)
  INTEGER :: Fun
  INTEGER :: Arg
    Fun = Arg
  END FUNCTION

