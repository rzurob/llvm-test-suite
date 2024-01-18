! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/CrossFeatures/ArrConstrPoly.f
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
!*    The selector is a nested poly arr constructor
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

  PROGRAM DerTypeArrConstrPoly

  USE M, DT=>Child
  IMPLICIT NONE

  CLASS(Base(4)), ALLOCATABLE :: V(:)

  ALLOCATE (V(3), SOURCE=Base(4)(Unknown=(/"123","123"/)) )

  ASSOCIATE( As => (/DT(4)(Base=V(1)),DT(4)(Base=V(2)) /) )

    IF ( ANY(SHAPE(As)  .NE. (/2/) ) ) STOP 20
    IF ( ANY(As%BaseID  .NE. 1 ) )     STOP 21
    IF ( ANY(As%ChildID .NE. 2 ) )     STOP 22

    IF ( As%Bnd(2)  .NE. 2  )          STOP 23

    SELECT TYPE ( As => As(2)%Unknown  )
    TYPE IS (CHARACTER(*))
      IF ( ANY(SHAPE(As) .NE. (/2/) ) )  STOP 30
      IF ( ANY(As        .NE. "123" ) )  STOP 31
    CLASS DEFAULT
      STOP 33
    END SELECT

  END ASSOCIATE

  END


