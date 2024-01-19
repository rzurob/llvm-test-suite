! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 20, 2005
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
!*   The selector is a host associate name associating to a
!*   poly dummy array of zero size
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
    CONTAINS
      PROCEDURE, NoPASS   :: Called
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    FUNCTION Called()
    LOGICAL :: Called
      Called =.true.
    END FUNCTION

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = 2
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = 1
    END FUNCTION

  END MODULE


  PROGRAM SltArrHostDummyAssumShp
  USE M
  IMPLICIT NONE
  CLASS(Base), POINTER :: V(:,:)

  ALLOCATE(V(-33:33,33:66), SOURCE=Child())

  CALL Sub(V(-1:0, 34:35))

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(Base), OPTIONAL :: Arg(20:, -10:)

    IF ( .NOT. PRESENT(Arg) ) ERROR STOP 11

    SELECT TYPE (U => Arg(::1,:))
    CLASS IS (Child)
    SELECT TYPE (W => U(1:2,:) )
    CLASS IS (Child)
      SELECT TYPE (V => W)
        TYPE IS (Child)

          IF ( SIZEOF(V)        .NE. 0 )          ERROR STOP 41
          IF ( SIZE(V)          .NE. 4 )          ERROR STOP 21
          IF ( ANY (LBOUND(V)   .NE. (/1, 1/) ) ) ERROR STOP 30
          IF ( ANY (UBOUND(V)   .NE. (/2, 2/) ) ) ERROR STOP 31
          IF ( ANY(SHAPE(V)     .NE. (/2,2/)) )   ERROR STOP 20

          IF ( .NOT. V%Called() ) ERROR STOP 45

       CLASS DEFAULT
          STOP 40
        CLASS is (Child)
          STOP 56
      END SELECT

  END SELECT
  END SELECT

  END SUBROUTINE


  END



