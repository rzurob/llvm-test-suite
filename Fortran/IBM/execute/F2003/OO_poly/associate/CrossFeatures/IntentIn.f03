! *********************************************************************
!*  ===================================================================
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
!*    The selector is a dummy with ontent(in)
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
      CLASS(*), ALLOCATABLE :: Unknown(:)
      CONTAINS
      PROCEDURE,nopass :: Bnd
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION Bnd(Arg)
    INTEGER, INTENT(IN) :: Arg
    INTEGER :: Bnd
      Bnd =Arg
    END FUNCTION

  END MODULE

  PROGRAM IntentIn

  USE M, DT=>Child
  IMPLICIT NONE

  CLASS(DT), POINTER :: V(:)

  ALLOCATE (V(3), SOURCE=DT(Unknown=(/"321","321"/)) )

  CALL Sub(V)

  IF (.NOT. ASSOCIATED(V))         ERROR STOP 50
  IF ( ANY(V%BaseID  .NE.  -1 ) )  ERROR STOP 51
  IF ( ANY(V%ChildID .NE.  -2 ) )  ERROR STOP 52

  SELECT TYPE ( As => V(1)%Unknown  )
    TYPE IS (CHARACTER(*))
      IF ( ANY(SHAPE(As) .NE. (/2/) ) )  ERROR STOP 60
      IF ( ANY(As        .NE. "321" ) )  ERROR STOP 61
    CLASS DEFAULT
      STOP 63
    END SELECT

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(DT), POINTER, INTENT(IN) :: Arg(:)

    ASSOCIATE( As => Arg )

      IF ( ANY(V%BaseID  .NE.  1 ) )  ERROR STOP 71
      IF ( ANY(V%ChildID .NE.  2 ) )  ERROR STOP 72

      As%BaseID = -1
      As%ChildID = -2

      IF ( ANY(V%BaseID  .NE.  -1 ) )  ERROR STOP 81
      IF ( ANY(V%ChildID .NE.  -2 ) )  ERROR STOP 82

  END ASSOCIATE

  END SUBROUTINE

  END

