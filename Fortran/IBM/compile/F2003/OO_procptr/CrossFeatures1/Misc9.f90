! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90 -qsuppress=1514-008
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp Misc9.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  Misc9.f
!*
!*  DATE                       : Jun. 08, 2005
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
!*  Objects with proc-ptr components are not allowed in default IO.
!*
!*  (ICE-304882)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc9
  IMPLICIT NONE

  TYPE :: DT
    INTEGER :: I=1
    PROCEDURE(), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  TYPE :: DT1
    SEQUENCE
    INTEGER :: I=1
    PROCEDURE(), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE


  TYPE(DT)  :: V1
  TYPE(DT1) :: V2

  READ *, V1
  PRINT *,V1

  READ *, V2
  PRINT *,V2


  END


