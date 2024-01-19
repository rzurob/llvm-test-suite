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
!* (ICE)
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

  PROGRAM final3
  USE M
  IMPLICIT CLASS(*)(U)

  TYPE :: Test
    LOGICAL :: V(3)
  END TYPE

  LOGICAL :: V(SIZE((/DT(),DT(),DT()/)))=.FALSE.

  !No Finalization shall heppen before the first executable stmta
  !since no function return in the specification expr.
  IF (ANY(Final .NEQV. .FALSE.)) ERROR STOP 20

  Final = .FALSE.
  CALL Sub(Test(V=V))

  CONTAINS

  SUBROUTINE  Sub(UArg)

  SELECT TYPE ( UArg )
  CLASS IS (Test)
    IF ( ANY(UArg%V) ) ERROR STOP 30
  CLASS DEFAULT
    STOP 61
  END SELECT
  SELECT TYPE ( UArg )
  CLASS IS (Test)
    IF ( ANY(UArg%V) ) ERROR STOP 31
  CLASS DEFAULT
    STOP 62
  END SELECT
  !No finalization shall happen

  END SUBROUTINE

  END


