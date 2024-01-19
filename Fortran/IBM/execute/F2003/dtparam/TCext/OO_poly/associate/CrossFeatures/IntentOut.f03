! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/CrossFeatures/IntentOut.f
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
!*    The selector is a dummy with ontent(out)
!*    ()
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

  PROGRAM IntentOut

  USE M, DT=>Child
  IMPLICIT NONE

  CLASS(DT(4)), POINTER :: V(:)

  ALLOCATE (V(3), SOURCE=DT(4)(Unknown=(/"321","321"/)) )

  CALL Sub(V)

  IF (.NOT. ASSOCIATED(V))        ERROR STOP 50
  IF ( ANY(V%BaseID  .NE. -1 ) )  ERROR STOP 51
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
  CLASS(DT(4)), POINTER, INTENT(OUT) :: Arg(:)

!  IF ( ASSOCIATED(Arg)) ERROR STOP 40 ! unpredicatble

  ALLOCATE (Arg(3), SOURCE=DT(4)(BaseID=-1, Unknown=(/"123","123"/)) )

  ASSOCIATE( As => (/DT(4)(Base=Arg(1)%Base, ChildID=-2), &
                     DT(4)(Base=Arg(2)%Base, ChildID=-2) /) )

    IF ( ANY(SHAPE(As)  .NE. (/2/) ) ) ERROR STOP 20
    IF ( ANY(As%BaseID  .NE. -1 ) )     ERROR STOP 21
    IF ( ANY(As%ChildID .NE. -2 ) )     ERROR STOP 22

    IF ( As%Bnd(2)  .NE. 2  )          ERROR STOP 23

    SELECT TYPE ( As => As(2)%Unknown  )
    TYPE IS (CHARACTER(*))
      IF ( ANY(SHAPE(As) .NE. (/2/) ) )  ERROR STOP 30
      IF ( ANY(As        .NE. "123" ) )  ERROR STOP 31
    CLASS DEFAULT
      STOP 33
    END SELECT

  END ASSOCIATE

  END SUBROUTINE

  END


