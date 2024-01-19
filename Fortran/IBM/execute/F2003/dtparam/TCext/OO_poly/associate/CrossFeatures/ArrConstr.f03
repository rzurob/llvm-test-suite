! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_poly/associate/CrossFeatures/ArrConstr.f
! opt variations: -qnol -qreuse=none

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
!*    The selector is a nested arr constructor
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

     TYPE, ABSTRACT :: Base(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: BaseId = 1
      CLASS(*), ALLOCATABLE :: Unknown(:)
      CONTAINS
      PROCEDURE,nopass :: Bnd
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (20,4)
      INTEGER(K1)  :: ChildId = 2
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION Bnd(Arg)
    INTEGER, INTENT(IN) :: Arg
    INTEGER :: Bnd
      Bnd =Arg
    END FUNCTION

  END MODULE

  PROGRAM DerTypeArrConstr
  USE M, DT=>Child
  IMPLICIT NONE

  call foo ((/DT(20,4)(Unknown=(/"123","123"/)),DT(20,4)(Unknown=(/"123","123"/)) /) )

  contains

!  ASSOCIATE( As => (/DT(20,4)(Unknown=(/"123","123"/)),DT(20,4)(Unknown=(/"123","123"/)) /) )
  subroutine foo (as)
    type(dt(*,4)), intent(in) :: as(:)

    IF ( ANY(SHAPE(As)  .NE. (/2/) ) ) ERROR STOP 20
    IF ( ANY(As%BaseID  .NE. 1 ) )     ERROR STOP 21
    IF ( ANY(As%ChildID .NE. 2 ) )     ERROR STOP 22

    IF ( As%Bnd(2)  .NE. 2  )          ERROR STOP 23

    SELECT TYPE ( As => As(2)%Unknown  )
    TYPE IS (CHARACTER(*))
      IF ( ANY(SHAPE(As) .NE. (/2/) ) )  ERROR STOP 30
      IF ( ANY(As        .NE. "123" ) )  ERROR STOP 31
    CLASS DEFAULT
      STOP 33
    END SELECT

  END subroutine

  END


