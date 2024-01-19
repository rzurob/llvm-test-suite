! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 13, 2005
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
!*  - should only be associated with subroutine
!*
!*  (Failed: implicit interface problem)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  IMPLICIT INTEGER(P)

  TYPE :: DT
    PROCEDURE(), POINTER, NOPASS :: ProcPtr
  END TYPE

  TYPE :: DT1
    PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr
  END TYPE

  LOGICAL L
  PROCEDURE(), POINTER :: ProcPtr

  CONTAINS

  FUNCTION F()
  INTEGER F
    L = .TRUE.
    F = 1
  END FUNCTION

  SUBROUTINE S()

    ProcPtr => F  ! Test if affect the definition of proc ptr component
    IF ( ProcPtr() .NE. 1 ) ERROR STOP 11

    L = .TRUE.
    PRINT*, "In Sub"
  END SUBROUTINE

  END MODULE

  USE M

  TYPE(DT)  :: U(1), V
  TYPE(DT1) :: W

  L = .FALSE.
  V%PRocPtr => S
  CALL V%ProcPtr()
  IF( .NOT. L) ERROR STOP 11

  L = .FALSE.
  U(1)%ProcPtr => S
  CALL U(1)%ProcPtr()
  IF( .NOT. L) ERROR STOP 12

  L = .FALSE.
  W%ProcPtr => F
  PRINT*, W%ProcPtr()
  IF( .NOT. L) ERROR STOP 13

END


