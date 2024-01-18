! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 02, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* Finalization
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890





  MODULE M
    TYPE  :: DT
      CHARACTER(513) :: C0="0"
      CONTAINS
      Final :: FinalDT
    END TYPE

    LOGICAL :: Final(10) = .FALSE.
    INTEGER :: Index = 0

    CONTAINS

    SUBROUTINE FinalDT(Arg)
    TYPE(DT) :: Arg
      Index = Index + 1
      Final(Index) = .TRUE.
    END SUBROUTINE

  END MODULE

  PROGRAM final4
  USE M
  IMPLICIT CLASS(*)(U)
  Type(DT), PARAMETER :: U=DT(C0="-0")
  TYPE(DT) :: V

  IF (ANY(Final .NEQV. .FALSE.)) STOP 20
  CALL Sub(V)
  IF (Final(1)      .NEQV. .TRUE.)   STOP 31
  IF (ANY(Final(2:) .NEQV. .FALSE.)) STOP 33

  CONTAINS

  SUBROUTINE  Sub(UArg)
  INTENT(OUT) :: UArg

  SELECT TYPE ( UArg )
  CLASS IS (DT)
    IF (Final(1)      .NEQV. .TRUE.)   STOP 41
    IF (ANY(Final(2:) .NEQV. .FALSE.)) STOP 43
  CLASS DEFAULT
    STOP 61
  END SELECT
  !No new finalization

  IF (Final(1)      .NEQV. .TRUE.)   STOP 51
  IF (ANY(Final(2:) .NEQV. .FALSE.)) STOP 53

  END SUBROUTINE

  END


