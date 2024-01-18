! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Optional6.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Optional5
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


  PROGRAM Optional6
  USE M
  IMPLICIT NONE

  CLASS(DT), ALLOCATABLE, TARGET  ::  DTV
  TYPE(DT),   POINTER   :: DTVPtr

  CALL Sub()
  CALL Sub(DTV)

    SELECT TYPE (U => DTV)
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, DT()))        STOP 50

      SELECT TYPE ( U )

      TYPE IS (DT)
        IF ( U%Id      .NE. DTV%Id  )      STOP 55
        IF ( U%GetId() .NE. DTV%GetId())   STOP 56

      CLASS DEFAULT
        STOP 57
      END SELECT

    END SELECT

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(DT), ALLOCATABLE, OPTIONAL  :: Arg

    IF ( .NOT. PRESENT(Arg)) RETURN

    ALLOCATE(Arg)

    SELECT TYPE (U => Arg)
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))        STOP 30

    ASSOCIATE ( W => U )

      SELECT TYPE (U => W )

      TYPE IS (DT)

        IF ( U%Id      .NE. DTV%Id  )      STOP 42
        IF ( U%GetId() .NE. DTV%GetId())   STOP 43

      CLASS DEFAULT
        STOP 51
      END SELECT

    END ASSOCIATE
    END SELECT

  END SUBROUTINE

  END




