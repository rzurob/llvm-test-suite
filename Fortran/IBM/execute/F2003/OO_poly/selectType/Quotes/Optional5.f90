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
!*  The associating entity's optional attribute
!*  dummy with optional and allocatable attributes
!*  (Wrong allocation status-298777)
!*
!234567890123456789012345678901234567890123456789012345678901234567890





  MODULE M
    TYPE :: DT
      INTEGER :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    CLASS(DT), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION
  END MODULE


  PROGRAM Optional5
  USE M
  IMPLICIT NONE

  CLASS(DT), ALLOCATABLE, TARGET  ::  DTV(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
  TYPE(DT),   POINTER   :: DTVPtr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)

  CALL Sub()
  CALL Sub(DTV)

    SELECT TYPE (U => DTV)
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, DT()))        STOP 50
      IF ( SIZE(U)          .NE. 2**18 )       STOP 51
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/) ) ) STOP 52
      IF ( ANY (UBOUND(U)   .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/) ) ) STOP 53
      IF ( ANY(SHAPE(U)     .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/)) )  STOP 54

      SELECT TYPE ( U )

      TYPE IS (DT)
        IF ( ANY(U%Id      .NE. DTV%Id ) )      STOP 55
        IF ( ANY(U%GetId() .NE. DTV%GetId()))   STOP 56

      CLASS DEFAULT
        STOP 57
      END SELECT

    END SELECT

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(DT), ALLOCATABLE, OPTIONAL  :: Arg(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
  INTEGER :: S(2)=(/1,2/)

    IF ( .NOT. PRESENT(Arg)) RETURN

    ALLOCATE(Arg(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2))

    SELECT TYPE (U => Arg(S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S))
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))        STOP 30
      IF ( SIZE(U)          .NE. 2**18 )      STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/)) )  STOP 34

    ASSOCIATE ( W => U )

      SELECT TYPE (U => W )

      TYPE IS (DT)

        DTVPtr => DTV

        IF ( ANY(U%Id      .NE. DTVPtr%Id ) )      STOP 42
        IF ( ANY(U%GetId() .NE. DTVPtr%GetId()))   STOP 43

      CLASS DEFAULT
        STOP 51
      END SELECT

    END ASSOCIATE
    END SELECT

  END SUBROUTINE

  END




