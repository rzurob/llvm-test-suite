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
! %POSTCMD: tcomp Misc27.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc27
!*
!*  DATE                       : Mar. 07, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*   Diagnosis on private component
!*    (ICE-300849)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT, PRIVATE ::  Zero
    END TYPE

    TYPE, EXTENDS(Zero) :: Base
      INTEGER, PRIVATE :: BaseId = 1
    END TYPE

  END MODULE

  PROGRAM Misc27
  USE M,  DT=>Base
  IMPLICIT NONE

  TYPE(DT), TARGET :: T=DT(BaseID=-1)

  END


