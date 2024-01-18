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
!* (299542)
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

  PROGRAM final5
  USE M
  IMPLICIT CLASS(*)(U)
  Type(DT), PARAMETER :: U=DT(C0="-0")
  TYPE(DT) :: V,W

  print*, Final
  print*, V%C0

  Final = .FALSE.
  Index = 0
  V = U

  IF (ANY(Final(1:1) .NEQV. .TRUE.))  STOP 21
  IF (ANY(Final(3: ) .NEQV. .FALSE.)) STOP 23

  Final = .FALSE.
  Index = 0
  CALL Sub(V)

  IF (ANY(Final(1:2) .NEQV. .TRUE. )) STOP 33
  IF (ANY(Final(3: ) .NEQV. .FALSE.)) STOP 34

  CONTAINS

  SUBROUTINE  Sub(UArg)

  SELECT TYPE ( UArg )
  TYPE IS (DT)
    IF (ANY(Final(:) .NEQV. .FALSE.)) STOP 41
    UArg = DT()
    IF (ANY(Final(1:2) .NEQV. .TRUE. )) STOP 42
    IF (ANY(Final(3: ) .NEQV. .FALSE.)) STOP 43
  CLASS DEFAULT
    STOP 61
  END SELECT

  !No new finalization
  IF (ANY(Final(1:2) .NEQV. .TRUE.))  STOP 53
  IF (ANY(Final(3: ) .NEQV. .FALSE.)) STOP 55

  END SUBROUTINE

  END


