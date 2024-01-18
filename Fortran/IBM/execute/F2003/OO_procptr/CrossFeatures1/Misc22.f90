! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  Misc22.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  Misc21.f
!*
!*  DATE                       : Jun. 14, 2005
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
!*  Procedure pointer component with implicit interface
!*
!*  (Failed: implicit interface problem)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  LOGICAL L

  CONTAINS

  FUNCTION F()
  INTEGER F
    L = .TRUE.
    F = 1
  END FUNCTION

  SUBROUTINE S()
    L = .TRUE.
    PRINT*, "In Sub"
  END SUBROUTINE

  END MODULE

  PROGRAM Misc22
  USE M
  IMPLICIT INTEGER(P)

  TYPE :: DT
    PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr
  END TYPE

  TYPE :: DT1
    PROCEDURE(), POINTER, NOPASS :: ProcPtr
  END TYPE


  TYPE(DT)  :: V
  TYPE(DT1) :: U

  L = .FALSE.
  V%PRocPtr => F
  PRINT *, V%ProcPtr()
  IF( .NOT. L) STOP 11

  L = .FALSE.
  U%ProcPtr => S
  CALL U%ProcPtr()
  IF( .NOT. L) STOP 12

END


