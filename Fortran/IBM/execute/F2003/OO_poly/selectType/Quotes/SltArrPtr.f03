! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 06, 2005
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
!*   The selector is an array(zero size) pointer
!*    (297764)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero
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


  PROGRAM SltArrPtr
  USE M
  IMPLICIT NONE
  TYPE(Child), TARGET :: Tar

  CLASS(Zero), POINTER :: Arr(:)

  ALLOCATE(Child :: Arr(1111))

  SELECT TYPE ( As => Arr(::2))
    CLASS IS (Child)
      SELECT TYPE (As => As(::3))
        CLASS DEFAULT
        SELECT TYPE ( As )
          TYPE IS (Child)

            IF ( SIZE(As)   .NE. 186 )          ERROR STOP 42
            IF ( SIZEOF(As) .NE. 0 )            ERROR STOP 43
            IF ( ANY(As%Base%GetId() .NE. 1 ) ) ERROR STOP 44
            IF ( ANY(As%GetId()      .NE. 2 ) ) ERROR STOP 45

         CLASS DEFAULT
            STOP 40
        END SELECT
      END SELECT

    TYPE is (Base)
      STOP 32
    TYPE IS (Zero)
      STOP 38

  END SELECT

  END
