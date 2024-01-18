! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/selectType/Quotes/BranchByExit.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 27, 2005
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
!*  Branch out by EXIT
!*
!234567890123456789012345678901234567890123456789012345678901234567890





  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    CLASS(DT(4)), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION
  END MODULE


  PROGRAM BranchByExit
  USE M
  IMPLICIT NONE

  TYPE(DT(4))  ::  DTV(3,3,3)

  CALL Sub(DTV)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(DT(4))  :: Arg(:,:,:)
  INTEGER :: S(3)=(/1,2,3/), I, J
  LOGICAL :: L(2)=.FALSE.

    DO i=1,1

6   SELECT TYPE (U => Arg(S,S,S))
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))        STOP 30
      IF ( SIZE(U)          .NE. 27 )         STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3,3,3/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/3,3,3/)) )  STOP 34

    ASSOCIATE ( W => U )

      DO j=1,1

2     SELECT TYPE (U => W )

      TYPE IS (DT(4))

        IF ( ANY(U%Id      .NE. DTV%Id ) )      STOP 42
        IF ( ANY(U%GetId() .NE. DTV%GetId()))   STOP 43
        EXIT

3     CLASS DEFAULT
        STOP 51
      END SELECT

      END DO
!     PRINT*, "OUT SELECT2"
      L(1) = .TRUE.

4   END ASSOCIATE

    EXIT
5   END SELECT

    END DO
    L(2) = .TRUE.
!   PRINT*, "OUT SELECT1"

    IF (.NOT. ALL(L)) STOP 55

  END SUBROUTINE

  END


