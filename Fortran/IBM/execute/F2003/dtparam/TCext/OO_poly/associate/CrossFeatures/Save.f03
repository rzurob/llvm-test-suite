! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/CrossFeatures/Save.f
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

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
      CLASS(*), ALLOCATABLE :: Unknown(:)
      CONTAINS
      PROCEDURE,nopass :: Bnd
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
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

  TYPE(DT(4)) :: V(3)

  V = DT(4)(ChildID=2, BaseID=1, Unknown=(/"123","123"/))

  CALL Sub(V)

  IF ( ANY(V%BaseID  .NE.  -1 ) )  ERROR STOP 51
  IF ( ANY(V%ChildID .NE.  -2 ) )  ERROR STOP 52

  CALL Sub(V)

  IF ( ANY(V%BaseID  .NE.  1 ) )  ERROR STOP 51
  IF ( ANY(V%ChildID .NE.  2 ) )  ERROR STOP 52

  SELECT TYPE ( As => V(1)%Unknown  )
  TYPE IS (CHARACTER(*))
    IF ( ANY(SHAPE(As) .NE. (/2/) ) )  ERROR STOP 60
    IF ( ANY(As        .NE. "123" ) )  ERROR STOP 61
  CLASS DEFAULT
    STOP 63
  END SELECT

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(DT(4))       :: Arg(3)
  TYPE(DT(4)), SAVE :: T(3)=DT(4)(ChildID=-2, BaseID=-1, Unknown=NULL())
  TYPE(DT(4))        :: Temp(3)

  ASSOCIATE ( As1 => T, As2 => Arg)
  SELECT TYPE (As2)
  TYPE IS (DT(4))

      Temp = As2
      As2  = As1
      As1  = Temp

  CLASS DEFAULT
    STOP 98
  END SELECT

  END ASSOCIATE

  END SUBROUTINE

  END


