! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 09, 2005
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
!*    The selector is an entity  with save
!*    (300942)
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

  PROGRAM Save

  USE M, DT=>Child
  IMPLICIT NONE

  TYPE(DT) :: V(3)

  V = DT(ChildID=2, BaseID=1, Unknown=(/"123","123"/))

  CALL Sub(V)

  IF ( ANY(V%BaseID  .NE.  -1 ) )  STOP 51
  IF ( ANY(V%ChildID .NE.  -2 ) )  STOP 52

  CALL Sub(V)

  IF ( ANY(V%BaseID  .NE.  1 ) )  STOP 51
  IF ( ANY(V%ChildID .NE.  2 ) )  STOP 52

  SELECT TYPE ( As => V(1)%Unknown  )
  TYPE IS (CHARACTER(*))
    IF ( ANY(SHAPE(As) .NE. (/2/) ) )  STOP 60
    IF ( ANY(As        .NE. "123" ) )  STOP 61
  CLASS DEFAULT
    STOP 63
  END SELECT

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(DT)       :: Arg(3)
  TYPE(DT), SAVE :: T(3)=DT(ChildID=-2, BaseID=-1, Unknown=NULL())
  TYPE(DT)        :: Temp(3)

  ASSOCIATE ( As1 => T, As2 => Arg)
  SELECT TYPE (As2)
  TYPE IS (DT)

      Temp = As2
      As2  = As1
      As1  = Temp

  CLASS DEFAULT
    STOP 98
  END SELECT

  END ASSOCIATE

  END SUBROUTINE

  END


