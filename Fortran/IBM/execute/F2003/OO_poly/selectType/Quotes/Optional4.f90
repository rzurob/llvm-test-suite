! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 25, 2005
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
!*  The associating entity's optional attribute.
!*  dummy array section with vector subscript
!*  ()
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


  PROGRAM Optional4
  USE M
  IMPLICIT NONE

  TYPE(DT),   TARGET   ::    DTV(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
  TYPE(DT),   POINTER  :: DTVPtr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)

  CALL Sub()
  CALL Sub(DTV)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*), TARGET, OPTIONAL  :: Arg(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
  INTEGER :: S(2)=(/1,2/)

    IF ( .NOT. PRESENT(Arg)) RETURN

    SELECT TYPE (U => Arg(S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S))
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))        ERROR STOP 30
      IF ( SIZE(U)          .NE. 2**18 )      ERROR STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/) ) ) ERROR STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/) ) ) ERROR STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/)) )  ERROR STOP 34

    ASSOCIATE ( W => U )

      SELECT TYPE (U => W )

      TYPE IS (DT)
        DTVPtr => DTV
        IF ( ANY(U%Id      .NE. DTVPtr%Id ) )      ERROR STOP 42
        IF ( ANY(U%GetId() .NE. DTVPtr%GetId()))   ERROR STOP 43

      CLASS DEFAULT
        STOP 51
      END SELECT

    END ASSOCIATE
    END SELECT

  END SUBROUTINE

  END



